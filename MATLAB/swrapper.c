/* == S-Function interface for the AUTOSAR simulator ==========================
 *
 * Oskar Abrahamsson <aboskar@chalmers.se> 
 *
 * This S-function communicates with the ARWrapper code over named pipes.
 * As of now, the named pipes are created here (and not in Haskell), but this
 * function does not call the ARWrapper code (although it can). More like a 
 * proof of concept. :-)
 *
 */
#include <fcntl.h>
#include <errno.h>
#include <libgen.h>
#include <stdbool.h>
#include <string.h>   
#include <sys/stat.h>
#include <sys/types.h> 
#include <unistd.h> 

#define S_FUNCTION_NAME swrapper
#define S_FUNCTION_LEVEL 2       // Leave this as 2
#include "simstruc.h"

#define P_DIE                       protocol_die
#define P_OK                        protocol_ok
#define MINIMUM_NEXT_VAR_HIT_OFFSET 1e-12

/* Very simple transfer protocol. */
char protocol_die[1] = {1};
char protocol_ok[1] = {0};

char   *path;
size_t hs_input_width   = 0;
size_t hs_output_width  = 0;
bool   sim_running      = false;
int    hs_input_fd;
int    hs_output_fd;
char   hs_input_fifo[]  = "/tmp/infifo";
char   hs_output_fifo[] = "/tmp/outfifo";
double next_hit         = 0.0;


/* == FUNCTION init_simulator =================================================
 * Sets up input and output FIFOs and waits for port width information from
 * the simulator.
 *
 * Reading on file descriptors (or sockets) is blocking, but we can still
 * quit Simulink as long as we're blocked in here (i.e. not in a foreign
 * call).
 *
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

    ssPrintf("= FIFOS are\n  Input: %s\n  Output: %s\n"
            , hs_input_fifo, hs_output_fifo);


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

      //-- Perform handshake ------------------------------------------------//
      ssPrintf("= Sending OK.\n");
      write(hs_input_fd, P_OK, 1);
      
      ssPrintf("= Waiting for size.\n");
      char buf_in[1] = {0};
      read(hs_output_fd, buf_in, 1);
      hs_input_width = (size_t) buf_in[0]; 
      ssPrintf("Haskell requested input size: %u\n", hs_input_width);

      ssPrintf("= Waiting for size.\n");
      char buf_out[1] = {0};
      read(hs_output_fd, buf_out, 1);
      hs_output_width = (size_t) buf_out[0]; 
      ssPrintf("Haskell requested output size: %u\n", hs_output_width);

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
  ssSetInputPortDirectFeedThrough(S, 0, 1);

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

  if (width != hs_input_width) {
    ssPrintf("AUTOSAR model expects an input port width of %u\n"
             "Tried to set %u\n", hs_input_width, width);
    ssSetErrorStatus(S, "Tried to set port width not matching that specified"
                        "by AUTOSAR model");
    return;
  }

  ssSetInputPortWidth(S, portIndex, width);
  return;
}

/* == FUNCTION mdlSetOutputPortWidth ==========================================
 * Called by Simulink to set a desired port width.
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

  if (width != hs_output_width) {
    ssPrintf("AUTOSAR model expects an output port width of %u\n"
             "Tried to set %u\n", hs_output_width, width);
    ssSetErrorStatus(S, "Tried to set port width not matching that specified"
                        "by AUTOSAR model");
    return;
  }

  ssSetOutputPortWidth(S, portIndex, width);
  return;

}

/* == FUNCTION mdlOutputs =====================================================
 * Triggers a step in the simulation. Called by Simulink whenever a simulation
 * step takes place.
 *
 * o  All MATLAB types are coerced into doubles.
 *
 * o  Input must be read elsewhere if we are to not use direct feedthrough
 *    (might need to, in order to support assymetric port in-/out widths).
 */
static void mdlOutputs(SimStruct *S, int_T tid)
{
  InputRealPtrsType uPtrs = ssGetInputPortRealSignalPtrs(S, 0);
  real_T*           outp  = ssGetOutputPortRealSignal(S, 0);
  size_t            width = (size_t) ssGetInputPortWidth(S, 0);
  
  // -- Put data on simulator input bus -------------------------------------//
  
  double time = (double) ssGetT(S);
  write(hs_input_fd, P_OK, 1);
  write(hs_input_fd, &time, sizeof(double));
  write(hs_input_fd, (char *) *uPtrs, hs_input_width * sizeof(double));

  // ------------------------------------------------------------------------//

  // -- Retrieve data from simulator output bus -----------------------------//

  char status;
  read(hs_output_fd, &status, 1);
  if (status != 0) {
    ssSetErrorStatus(S, "Simulator requested death, halting.");
    sim_running = false;
    return;
  }

  read(hs_output_fd, (char *) &next_hit, sizeof(double));
  read(hs_output_fd, (char *) outp, hs_output_width * sizeof(double));

  // ------------------------------------------------------------------------//

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
  if (sim_running) write(hs_input_fd, P_DIE, 1);

  //-- Reset everything -----------------------------------------------------//
  hs_input_width  = 0;
  hs_output_width = 0;
  next_hit        = 1e-1; 
  sim_running     = false;
  
  /* Close file descriptors, not terribly interested if this succeeds
   * or not as we're done anyway.
   */
  close(hs_input_fd);
  close(hs_output_fd);

  unlink(hs_input_fifo);
  unlink(hs_output_fifo);

  ssPrintf("= Unlinked FIFOs and reset model.\n");

}

#ifdef MATLAB_MEX_FILE
#include "simulink.c"  // MEX interface mechanism
#else
#include "cg_sfun.h"   // Code generation registration function
#endif

