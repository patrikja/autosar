# ARSIM MATLAB utils

Utilities for connecting Simulink to ARSIM through the pipes interface.

## Contents

| File           | Comment                                                     |
| -------------- | ----------------------------------------------------------- | 
| ABS_demo.slx   | ABS demo model. Requires [arsim-examples](../arsim-examples)|
| ABS_dict.sldd  | ABS demo variable dictionary.                               |
| ACC_demo.slx   | ACC demo model. Requires [arsim-examples](../arsim-examples)|
| ACC_dict.sldd  | ACC demo variable dictionary.                               | 
|                |                                                             |
| debugm.h       | Some debug macros used by [protocol.c](protocol.c)          | 
| protocol.c     | Transfer protocol for communicating with ARSIM.             |
| protocol.h     |                                                             |
| swrapper.c     | S-function for including AUTOSAR components.                |
|                |                                                             |
| setMaskLabel.m | Script for setting mask labels, called by the S-function.   |

## Preliminaries

The code relies on named UNIX pipes (although sockets probably could be used in 
place with very little modification); I'm just assuming that it won't run under
Windows (I don't have access to a Windows machine) although you are free to try.

Ensure MATLAB is installed with the following software packages (this is my
setup, so it should work for you):

```
----------------------------------------------------------------------------------------------------
MATLAB Version: 9.0.0.341360 (R2016a)
MATLAB License Number: 650541
Operating System: Mac OS X  Version: 10.11.6 Build: 15G31 
Java Version: Java 1.7.0_75-b13 with Oracle Corporation Java HotSpot(TM) 64-Bit Server VM mixed mode
----------------------------------------------------------------------------------------------------
MATLAB                                                Version 9.0         (R2016a)
Simulink                                              Version 8.7         (R2016a)
Simscape                                              Version 4.0         (R2016a)
Simscape Driveline                                    Version 2.10        (R2016a)
Simscape Electronics                                  Version 2.9         (R2016a)
Simscape Fluids                                       Version 2.0         (R2016a)
Simscape Multibody                                    Version 4.8         (R2016a)
Simscape Power Systems                                Version 6.5         (R2016a)
Simulink Coder                                        Version 8.10        (R2016a)
```

Performing code generationg *might* require some extra MATLAB packages. In that
case it's possible to add packages post-installation.

## Setting up MEX

Before running any examples you will need to set up the 
[mex](http://se.mathworks.com/help/matlab/ref/mex.html) script with which you'll 
build the S-function (C-code). On OS X, install Xcode with command line tools.
On Linux, GCC 4 is supported. MATLAB should autodetect your settings in most
cases, and running `mex -setup` from MATLAB should provide you with something 
similar to:

   
    >> mex -setup
    MEX configured to use 'Xcode with Clang' for C language compilation.
   

## Building the C code

Provided that MEX has been set up properly, build the S-function from MATLAB: 

    >> mex -DNDEBUG swrapper.c protocol.c
    Building with 'Xcode with Clang'.
    MEX completed successfully.

## Running a Simulink model

Examples are located in the [arsim-examples](../arsim-examples) package. Once
built, pick either the [ABS](ABS_demo.slx) or [ACC](ACC_demo.slx) example, and
double-click the S-function block (these will be named "NewABS" or "ACC"). From
here you set the *absolute* path to whatever executable you just built.
(Remember the ' ticks or MATLAB won't know it's a string).

## Plotting, logging, et cetera 

Plotting can be done by inserting and connecting *Scope* blocks in Simulink
(connect these to whatever signal you wish to track). Scopes slows down
simulation, and are reset when the model restarts, however. Another alternative
is to click (when the simulation is stopped) the signal line which you would
like to track. A small blue pop-up will appear, with options allowing you to
log- or graph the signal. Signals can be accessed during or after simulation
from the drop-down menu **Simulation -> Output -> Simulation Data Inspector**.

## Creating your own model

Making the S-function variable in the number of ports provided tricky (and
unneccessary, since it's mostly bling). For this reason the S-function exports
single variable-width input and output ports. We still decided to support
putting labels on these (creating models required some guesswork prior to this). 
To this end the S-function block must be wrapped in a *mask* in Simulink. The 
easiest way to achieve this is to copy one of the S-function blocks from either
of the ACC or ABS models. Mux/demux outputs and block ports will need to be 
created and connected manually within Simulink (you will be provided with warning 
messages if you fail).

