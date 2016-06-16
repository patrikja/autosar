# ARSIM MATLAB utils

Utilities for connecting Simulink to ARSIM through the pipes interface.

## Contents

| File           | Comment                                                   |
| -------------- | --------------------------------------------------------- | 
| ACCDict.sldd   | Variable dictionary for the ACC demo.                     |
| ACCModel.slx   | ACC demo model. Requires the ACC code to be built.        |
| CarDict.sldd   | Variable dictionary for the ABS demo.                     |
| CarModel.slx   | ABS demo model. Requires the NewABS code to be built.     | 
|                |                                                           |
| makeMEX.m      | Make script for building the S-function.                  |
|                |                                                           |
| debugm.h       | Some debug macros used by `protocol.c`                    | 
| protocol.c     | Transfer protocol for communicating with ARSIM.           |
| protocol.h     |                                                           |
| swrapper.c     | S-function for including AUTOSAR components.              |
|                |                                                           |
| setMaskLabel.m | Script for setting mask labels.                           |

## Instructions

Build the S-function in MATLAB using the `makeMEX` script:
    >> makeMEX
    Building with 'Xcode with Clang'.
    MEX completed successfully.

Examples are located in `CarModel.slx` (ABS demo) and `ACCModel.slx` (ACC demo,
work in progress). You will need to supply the S-function with a path to the
compiled AUTOSAR component (compiled using `simulateUsingExternal` etc) and
**manually** set the number of inputs/outputs (MATLAB will warn you when you're
doing it wrong).

When creating new models, the S-function `swrapper` should be masked in a
similar way to what has been done in the demos in order for the C code to be
able to set port labels, etc. Ideally you just copy the masked component from
either of the demos and include in your model.

## Simulink models

The ACC demo requires that you build [the ACC example](../ACC/). The ABS demo
requires that you build [NewABS](../NewABS/).


