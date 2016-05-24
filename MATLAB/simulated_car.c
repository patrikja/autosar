/* == C implementation of simulated_car.m =====================================
 * 
 * Used to see if running interpreted MATLAB code had any larger effect on the
 * AUTOSAR simulation.
 */

#define S_FUNCTION_NAME simulated_car
#define S_FUNCTION_LEVEL 2
#include "simstruc.h"

double velo_in[4]  = {18.0, 18.0, 18.0, 18.0};
double accel_in[4] = {0.0, 0.0, 0.0, 0.0};
double nextT       = 1e-1;

void wheel_f(int, double, double*, double*);

static void mdlInitializeSizes(SimStruct *S)
{
  ssSetNumSFcnParams(S, 0);
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

#define MDL_INITIALIZE_SAMPLE_TIMES
static void mdlInitializeSampleTimes(SimStruct *S)
{
  ssSetSampleTime(S, 0, VARIABLE_SAMPLE_TIME);
  ssSetOffsetTime(S, 0, 0.0);
}

void mdlOutputs(SimStruct *S, int_T tid) 
{

  InputRealPtrsType uPtrs = ssGetInputPortRealSignalPtrs(S, 0);
  real_T*           outp  = ssGetOutputPortRealSignal(S, 0);
  int_T             width = ssGetOutputPortWidth(S, 0);
  double            time  = ssGetT(S);

  for (int i = 0; i < 4; i++) {
    wheel_f(i + 1, time, &velo_in[i], &accel_in[i]);
    outp[2*i] = velo_in[i];
    outp[2*i+1] = accel_in[i];
  }

  nextT += 1e-2;
}

double velo_step(double a, double v)
{
  double dt = 1e-2;
  return v + a * dt;
}

void wheel_f(int idx, double time, double *vel, double *acc)
{
  if (time < 1.0) {
    *vel = velo_step(0, *vel);
    *acc = 0;
    return;
  } else if (idx != 2) {
    *vel = velo_step(-4.5, *vel);
    *acc = -4.5;
    return;
  } else if (time < 1.6) {
    *vel = velo_step(-10, *vel);
    *acc = -10;
    return;
  } else if (time < 2) {
    *vel = velo_step(-4, *vel);
    *acc = -4;
    return;
  } else if (time < 2.5) {
    *vel = velo_step(-3, *vel);
    *acc = -3; 
    return;
  } else if (time < 3) {
    *vel = velo_step(0, *vel);
    *acc = 0;
    return;
  } else if (time < 3.4) {
    *vel = velo_step(-4.5, *vel);
    *acc = -4.5;
    return;
  } else if (time < 4) {
    *vel = velo_step(-5, *vel);
    *acc = -5; 
    return;
  } else if (time < 4.3) {
    *vel = velo_step(-8.4, *vel);
    *acc = -8.4;
    return;
  } else if (time < 4.7) {
    *vel = velo_step(-4, *vel); 
    *acc = -4;
    return;
  }
  *vel = velo_step(0, *vel);
  *acc = 0;
  
}

#define MDL_GET_TIME_OF_NEXT_VAR_HIT
static void mdlGetTimeOfNextVarHit(SimStruct *S)
{
  ssSetTNext(S, nextT);
}

static void mdlTerminate(SimStruct *S) {
  for (int i = 0; i < 4; i++) {
    velo_in[i]  = 18.0;
    accel_in[i] = 0.0;
  }
  nextT       = 0.1;
}

#ifdef MATLAB_MEX_FILE
#include "simulink.c"  // MEX interface mechanism
#else
#include "cg_sfun.h"   // Code generation registration function
#endif

