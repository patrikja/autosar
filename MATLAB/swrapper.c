/* == S-Function interface for the AUTOSAR simulator ==========================
 *
 * Oskar Abrahamsson, <aboskar@chalmers.se> 
 *
 * S-function wrapper for ARSIM. Connects to the pipe-based interface of ARSIM
 * allowing for Simulink to replace external components in the simulated AUTOSAR 
 * system.
 *
 * Simulink calling order, for future reference:
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * 1. mdlInitializeSizes
 * 2. [Various things such as port widths etc]
 * 
 * Simulation loop:
 *   3. mdlGetTimeOfNextVarHit
 *   4. mdlOutputs
 *   5. mdlUpdate
 *
 * The backwards order of the simulation loop means we need to run
 * ARSim with empty inputs one time before proceeding with regular
 * simulation. This is done in init_simulator() which is called from
 * mdlInitializeSizes.
 *
 */
#include <fcntl.h>
#include <errno.h>
#include <libgen.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>   
#include <sys/stat.h>
#include <sys/types.h> 
#include <unistd.h> 

#include "debugm.h"
#include "protocol.h"

#define S_FUNCTION_NAME swrapper
#define S_FUNCTION_LEVEL 2
#include "simstruc.h"

#define MINIMUM_DELTA 1e-6

/* Transfer protocol state */
Protocol *protocol;

/* S-function state */
char   *path;
bool   sim_running      = false;
int    hs_input_fd;
int    hs_output_fd;
char   hs_input_fifo[]  = "/tmp/infifo";  // PARAMETER?
char   hs_output_fifo[] = "/tmp/outfifo"; // PARAMETER?
double delta_time       = MINIMUM_DELTA;
double *intermediate;

void init_protocol(SimStruct *S);

/* == FUNCTION init_simulator =================================================
 * Sets up input and output FIFOs and waits for port width information from
 * the simulator.
 *
 * Reading on file descriptors (or sockets) is blocking, but we can still
 * quit Simulink as long as we're blocked in here (i.e. not in a foreign
 * call).
 *
 * TODO: Fix the string command allocations which are arbitrarily declared to 
 *       a length of 256.
 */
void init_simulator(SimStruct *S) 
{
  if (!sim_running) {
    //-- Open FIFOs ---------------------------------------------------------//
    
    /* There are two ways to run the Simulink model. One way is to run the simulation
     * from Simulink directly (for example by using the MATLAB gui). In this case
     * it's the Simulink C stub that creates named pipes and launches the
     * Haskell AUTOSAR simulator. The second way is to run the Haskell
     * 'driver', which sets up the environment (the named pipes) and
     * launches the Simulink model. The Simulink C stub realizes that it
     * should not set up the environment by looking at the environment variable
     * ARSIM_DRIVER.*/
    bool arsim_driver = (getenv("ARSIM_DRIVER") != NULL);

    if(!arsim_driver) {
      if (mkfifo(hs_input_fifo, 0666) < 0) {
        ssSetErrorStatus(S, "Error creating Haskell input FIFO.");
        return;
      }

      if (mkfifo(hs_output_fifo, 0666) < 0) {
        ssSetErrorStatus(S, "Error creating Haskell output FIFO.");
        return;
      }

      //-- Get simulator path and attempt system call -------------------------//

      size_t buflen = mxGetN(ssGetSFcnParam(S, 0)) * sizeof(mxChar) + 1;
      path = malloc(buflen);
      if (mxGetString(ssGetSFcnParam(S, 0), path, buflen)) {
        ssSetErrorStatus(S, "Error getting path string parameter.");
        return;
      }
   
      /* Check that path exists, and that it points to something
       * that is an executable regular file (i.e. not a directory).
       */
      struct stat sb;
      bool is_exec = stat(path, &sb) == 0 
                  && sb.st_mode & S_IXUSR 
                  && S_ISREG(sb.st_mode);
    
      if (!is_exec) {
        ssSetErrorStatus(S, "Path does not point to a valid executable file.");  
        return;
      }

      char *name = basename(path);
      if (name == NULL) {
        ssSetErrorStatus(S, "Call to basename() failed.");
        return;
      }
      char *const argv[] = {name, hs_input_fifo, hs_output_fifo, NULL};
     
      pid_t child = fork();
      
      if (child < 0) {
        ssSetErrorStatus(S, "Call to fork() failed.");
        return;
      }

      if (child > 0) {
        init_protocol(S);

      }
      
      if (child == 0) execv(path, argv); 
    } else {
      init_protocol(S);
    }
  
  }

  return; 
}

void init_protocol(SimStruct *S) 
{
  /* Open file descriptors. 
   * 
   * NOTE: This must be done /after/ the fork, or there'll be issues with
   * the forked process. Likely, these things are inherited.
   */
  hs_input_fd  = open(hs_input_fifo, O_WRONLY);
  hs_output_fd = open(hs_output_fifo, O_RDONLY);
  if (hs_input_fd < 0 || hs_output_fd < 0) {
    ssSetErrorStatus(S, "Error opening file descriptor.");
    return;
  }

  //-- Init protocol struct and call handshake procedure ----------------// 
  protocol = Protocol_init(hs_input_fd, hs_output_fd);
  if (protocol == NULL) {
    ssPrintf("Protocol_init returned NULL pointer.\n");
    ssSetErrorStatus (S, "Protocol_init: PROTOCOL_MEM_ERROR");
    return;
  }
  
  switch(Protocol_handshake(protocol)) {
    case PROTOCOL_MEM_ERROR:
      free(protocol);
      ssSetErrorStatus(S, "Protocol_handshake: PROTOCOL_MEM_ERROR");
      return;
    case MEM_ERROR:
      free(protocol);
      ssSetErrorStatus(S, "Protocol_handshake: MEM_ERROR");
      return;
    case PROTOCOL_ERROR:
      free(protocol);
      ssSetErrorStatus(S, "Protocol_handshake: PROTOCOL_ERROR");
      return;
    case PROTOCOL_SUCCESS:
    default: ;
  }

  /* Set mask port labels. 
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   *
   * XXX NOTE:
   *     Decided on a fixed command size of 256 until I figure this out. If
   *     a 256-byte string is used the null-termination imposed by calloc
   *     will fail and we'll get a segfault.
   * XXX
   */
  const char *block_label = ssGetPath(S);

#ifdef MATLAB_MEX_FILE
/* This code adds labels to the model and prevents
* generated C code from compiling */
  for (uint8_t i = 0; i < protocol->p_input_labels; i++) {
    char *cmd = calloc(256, sizeof(char));
    sprintf(cmd, "setMaskLabel('%s', '%s', %u, '%s', %u);", 
            block_label, protocol->p_input_labels_str[i], i + 1,
            "input", i == 0);
    mexEvalString(cmd);
    free(cmd);
  }
  
  for (uint8_t i = 0; i < protocol->p_output_labels; i++) {
    char *cmd = calloc(256, sizeof(char));
    sprintf(cmd, "setMaskLabel('%s', '%s', %u, '%s', %u);", 
            block_label, protocol->p_output_labels_str[i], i + 1,
            "output", 0);
    mexEvalString(cmd);
    free(cmd);
  }
#endif

  /* - Set up some intermediate storage (MATLAB crux). 
   * - Run the simulator one step (need some outputs to be ready since
   *   Simulink updates the model in a backwards order).
   */
  intermediate = calloc(protocol->p_input_width, sizeof(double));
  if (intermediate == NULL) {
    ssSetErrorStatus(S, "Memory allocation error in init_simulator.");
    return;
  }
  Protocol_send_data(protocol, intermediate);

  sim_running = true;
}


/* == FUNCTION mdlInitializeSizes =============================================
 * Initialize IO sizes for the S-Function port interface.
 *
 * The port widths are re-set after the AUTOSAR model has been queried, leave
 * as dynamically sized.
 *
 * NOTE: This is the first function call performed by Simulink during model
 *       initialization. Because of this, we perform ARSim initialization here.
 */
static void mdlInitializeSizes(SimStruct *S)
{
  // Easier to spot a restart this way.
  fprintf(stderr, 
      "*****************************************"
      "***************************************\n"
      "* ARSim Simulink wrapper                 "
      "                                      *\n"
      "*****************************************"
      "***************************************\n");
  debug("( trace )");

  ssSetNumSFcnParams(S, 1);
  if (ssGetNumSFcnParams(S) != ssGetSFcnParamsCount(S)) {
    return;
  }

  if (!ssSetNumInputPorts(S, 1)) return;
  ssSetInputPortWidth(S, 0, DYNAMICALLY_SIZED);

  if (!ssSetNumOutputPorts(S, 1)) return;
  ssSetOutputPortWidth(S, 0, DYNAMICALLY_SIZED);

  ssSetOptions(S, SS_OPTION_EXCEPTION_FREE_CODE);
  ssSetOptions(S, SS_OPTION_CALL_TERMINATE_ON_EXIT);
  
  init_simulator(S);
}

/* == FUNCTION mdlInitializeSampleTimes =======================================
 * Initialize sample times for the simulator.
 * 
 * We employ a variable discrete sample time. This requires that the simulator
 * provides this S-Function with a desired next sample hit. This data is placed
 * on the output bus from Haskell land. 
 */
#define MDL_INITIALIZE_SAMPLE_TIMES
static void mdlInitializeSampleTimes(SimStruct *S)
{
  ssSetSampleTime(S, 0, VARIABLE_SAMPLE_TIME);
  ssSetOffsetTime(S, 0, 0.0);
}


/* == FUNCTION mdlSetInputPortWidth ===========================================
 * Called by Simulink to set a desired port width.
 *
 * Cross checks against data communicated from the Haskell AUTOSAR model. If 
 * port width is legal (i.e. does not exceed that of the AUTOSAR model) a new 
 * port width is set, otherwise fails.
 *
 */
#define MDL_SET_INPUT_PORT_WIDTH
void mdlSetInputPortWidth(SimStruct *S, int portIndex, int width)
{
  if (width != protocol->p_input_width) {
    ssPrintf("AUTOSAR model expects an input port width of %u\n"
             "Tried to set %u\n", protocol->p_input_width, width);
    ssSetErrorStatus(S, "Tried to set port width not matching that specified"
                        "by AUTOSAR model");
    return;
  }

  ssSetInputPortWidth(S, portIndex, width);
  return;
}

/* Called by Simulink to set a desired port width.
 *
 * Cross checks against data communicated from the Haskell AUTOSAR model. If 
 * port width is legal (i.e. does not exceed that of the AUTOSAR model) a new 
 * port width is set, otherwise fails.
 */
#define MDL_SET_OUTPUT_PORT_WIDTH
void mdlSetOutputPortWidth(SimStruct *S, int portIndex, int width)
{
  if (width != protocol->p_output_width) {
    ssPrintf("AUTOSAR model expects an output port width of %u\n"
             "Tried to set %u\n", protocol->p_output_width, width);
    ssSetErrorStatus(S, "Tried to set port width not matching that specified"
                        "by AUTOSAR model");
    return;
  }

  ssSetOutputPortWidth(S, portIndex, width);
  return;

}

/* == FUNCTION mdlUpdate ======================================================
 * Called by Simulink to update model in every major integration step.
 * Sends data to Haskell, called prior to mdlOutputs.
 *
 * Input variables from Simulink can be of all kinds of types. Performing a 
 * memcpy or read from *uPtrs directly leads to all kinds of weird results,
 * hence the need for a scratchpad (*intermediate).
 */
#define MDL_UPDATE
static void mdlUpdate(SimStruct *S, int_T tid)
{
  debug("( trace )");

  InputRealPtrsType uPtrs = ssGetInputPortRealSignalPtrs(S, 0);

  for (size_t i = 0; i < protocol->p_input_width; i++) 
    intermediate[i] = *uPtrs[i];

  Protocol_send_data(protocol, intermediate);
}

/* == FUNCTION mdlOutputs =====================================================
 * Triggers a step in the simulation. Called by Simulink whenever a simulation
 * step takes place.
 */
static void mdlOutputs(SimStruct *S, int_T tid)
{
  debug("( trace )");

  real_T* outp  = ssGetOutputPortRealSignal(S, 0);
  size_t  width = (size_t) ssGetInputPortWidth(S, 0);
  
  switch(Protocol_get_data(protocol, &delta_time, outp)) {
    case PROTOCOL_SUCCESS: 
      break;
    case PROTOCOL_HALT: 
      sim_running = false;
      Protocol_destroy(protocol);
      ssSetErrorStatus(S, "Simulator requested death, halting.");
      return;
    default:
      ssSetErrorStatus(S, "PROTOCOL_FAILURE");
      return;
  }
}

/* == FUNCTION mdlGetTimeOfNextVarHit =========================================
 * Set time of next sample hit with variable sample time.
 */
#define MDL_GET_TIME_OF_NEXT_VAR_HIT
static void mdlGetTimeOfNextVarHit(SimStruct *S)
{
  debug("( trace )");

  if (delta_time == 0) {
    debug("Received a DELTA step of zero, progressing time by %e."
         , MINIMUM_DELTA);
    delta_time = MINIMUM_DELTA;
  }

  ssSetTNext(S, ssGetT(S) + (time_T) delta_time);
}

/* == FUNCTION mdlTerminate ===================================================
 * Called by Simulink whenever the simulation finishes. 
 *
 * Closes handles, unlinks FIFOs and resets model.
 */
static void mdlTerminate(SimStruct *S) {
  
  //-- Signal death, if not dead --------------------------------------------//
  if (sim_running) {
    write(hs_input_fd, P_DIE, 1);
    Protocol_destroy(protocol);
  }

  //-- Reset everything -----------------------------------------------------//
  delta_time      = MINIMUM_DELTA;
  sim_running     = false;
  free(intermediate);
  
  bool arsim_driver = (getenv("ARSIM_DRIVER") != NULL);

  if(!arsim_driver) {
    /* Close file descriptors, not terribly interested if this succeeds
     * or not as we're done anyway.
     */
    close(hs_input_fd);
    close(hs_output_fd);

    unlink(hs_input_fifo);
    unlink(hs_output_fifo);
  }

}

#ifdef MATLAB_MEX_FILE
#include "simulink.c"  // MEX interface mechanism
#else
#include "cg_sfun.h"   // Code generation registration function
#endif

