/* == S-Function interface for the AUTOSAR simulator ==========================
 *
 * Oskar Abrahamsson, <aboskar@chalmers.se> 
 *
 * S-function wrapper for ARSIM. Connects to the pipe-based interface of ARSIM
 * allowing for Simulink to replace external components in the simulated AUTOSAR 
 * system.
 *
 * KNOWN ISSUES
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *   o  There is an issue when simulating systems with a unit
 *      (output) port width, causing ARSIM to interpret the
 *      first data transfer as a request to halt simulation.
 *      The handshake and transfer protocols (or perhaps the 
 *      magic numbers involved) should be revised to prevent
 *      this.
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

#include "protocol.h"

#define S_FUNCTION_NAME swrapper
#define S_FUNCTION_LEVEL 2       // Leave this as 2
#include "simstruc.h"

#define MINIMUM_NEXT_VAR_HIT_OFFSET 1e-12

/* Transfer protocol state */
Protocol *protocol;

/* S-function state */
char   *path;
bool   sim_running      = false;
int    hs_input_fd;
int    hs_output_fd;
char   hs_input_fifo[]  = "/tmp/infifo";  // PARAMETER?
char   hs_output_fifo[] = "/tmp/outfifo"; // PARAMETER?
double next_hit         = 0.0;
bool   model_updated    = false;


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
       * There seems to be no right way to do this, but the Mask objects provide 
       * us with a 'port_label' command which can be used the following way in
       * the mask initialization:
       *
       *   port_label(''output'', 1, ''some_label'');
       *
       * The above sets the label text for the first output port in the mask to
       * 'some_label' (w/o ticks). The code can be executed in the mask context
       * of the correct block by fetching the block path using 'ssGetPath(S)' on
       * the SimStruct and calling the following
       *
       *   set_param(block_path, 'MaskDisplay', 'port_label(....)');
       *
       * from MATLAB. In order to make MATLAB evaluate it from here, we can 
       * create the above string and call mexEvalString().
       *
       * XXX NOTE:
       *     Decided on a fixed command size of 256 until I figure this out. If
       *     a 256-byte string is used the null-termination imposed by calloc
       *     will fail and we'll get a segfault.
       * XXX
       */
      const char *block_label = ssGetPath(S);

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

      sim_running = true;
    }
    
    if (child == 0) execv(path, argv); 
  
  }

  return; 
}

/* == FUNCTION mdlInitializeSizes =============================================
 * Initialize IO sizes for the S-Function port interface.
 *
 * The port widths are re-set after the AUTOSAR model has been queried, leave
 * as dynamically sized.
 */
static void mdlInitializeSizes(SimStruct *S)
{
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
  init_simulator(S);
  if (!sim_running) return;

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
  init_simulator(S);
  if (!sim_running) return;

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
 * TODO: The dynamic allocation when sending data is a quick fix. Rewriting
 *       Protocol_send_data and receiveVector (Haskell) to send/receive on a
 *       per-byte basis feels stupid in case we'd like to hook up the Haskell 
 *       code with something else at some point.
 */
#define MDL_UPDATE
static void mdlUpdate(SimStruct *S, int_T tid)
{
  // Set flag.
  if (!model_updated) model_updated = true;

  InputRealPtrsType uPtrs = ssGetInputPortRealSignalPtrs(S, 0);

  /* XXX QUICK-FIX XXX */
  double *data = malloc(protocol->p_input_width * sizeof(double));
  for (size_t i = 0; i < 8; i++) data[i] = *uPtrs[i];
  
  double time = (double) ssGetT(S);
  Protocol_send_data(protocol, time, data);

  /* XXX QUICK-FIX XXX */
  free(data);
}

/* == FUNCTION mdlOutputs =====================================================
 * Triggers a step in the simulation. Called by Simulink whenever a simulation
 * step takes place.
 *
 * TODO: Fix the zero-output unless model updated. This should _always_ be the
 * case; revise protocol.
 */
static void mdlOutputs(SimStruct *S, int_T tid)
{
  real_T* outp  = ssGetOutputPortRealSignal(S, 0);
  size_t  width = (size_t) ssGetInputPortWidth(S, 0);
  
  /* Unless mdlUpdate has been done, just put zeroes on the outport. */
  if (!model_updated) {
    double *tmp = calloc(protocol->p_output_width, sizeof(double));
    memcpy(outp, tmp, protocol->p_output_width * sizeof(double));
    free(tmp);
    return;
  }
  
  switch(Protocol_get_data(protocol, &next_hit, outp)) {
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
 * Set time of next sample hit with variable sample time. Time of next hit 
 * must EXCEED the current time given by ssGetT().
 *
 * TODO: There is a mishap right now where this is called before the first 
 * time we have triggered the simulator (and thus the output bus contains a
 * zero at the head). This is currently solved by incrementing time by a very
 * small amount set by MINIMUM_NEXT_VAR_HIT_OFFSET.
 */
#define MDL_GET_TIME_OF_NEXT_VAR_HIT
static void mdlGetTimeOfNextVarHit(SimStruct *S)
{
  time_T min_step = ssGetT(S) + MINIMUM_NEXT_VAR_HIT_OFFSET;
  time_T requested_time = (time_T) next_hit; 

  // MATLAB calls this before mdlOutputs. Right now we shift time by a very 
  // small amount but it means the time in the simulator is wrong by this
  // amount the first step. This should be avoided if possible.
  if (requested_time <= ssGetT(S)) {
#ifndef NDEBUG
    ssPrintf("error: incorrect time of %.8f requested (not exceeding "
             "%.8f). Progressing with minimum step.\n", 
             requested_time, ssGetT(S));
#endif
    ssSetTNext(S, min_step);
    return;
  }

  ssSetTNext(S, requested_time);
}

/* == FUNCTION mdlTerminate ===================================================
 * Called by Simulink whenever the simulation finishes. 
 *
 * Closes handles, unlinks FIFOs, resets model and checks errno.
 */
static void mdlTerminate(SimStruct *S) {
  
  //-- Signal death, if not dead --------------------------------------------//
  if (sim_running) {
    write(hs_input_fd, P_DIE, 1);
    Protocol_destroy(protocol);
  }

  //-- Reset everything -----------------------------------------------------//
  next_hit        = 1e-1; 
  sim_running     = false;
  model_updated   = false;
  
  /* Close file descriptors, not terribly interested if this succeeds
   * or not as we're done anyway.
   */
  close(hs_input_fd);
  close(hs_output_fd);

  unlink(hs_input_fifo);
  unlink(hs_output_fifo);

#ifndef NDEBUG
  ssPrintf("= Unlinked FIFOs and reset model.\n");
#endif

}

#ifdef MATLAB_MEX_FILE
#include "simulink.c"  // MEX interface mechanism
#else
#include "cg_sfun.h"   // Code generation registration function
#endif

