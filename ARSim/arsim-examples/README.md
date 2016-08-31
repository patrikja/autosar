# arsim-examples

Some examples written in the AUTOSAR DSL.

## Installation

The package requires that [arsim](../ARSim) is installed somewhere where 
cabal can find it. Using the `add-source` option to `cabal sandbox` seems to be 
the easiest way to achieve this:

    $ cd arsim-examples
    $ cabal sandbox init
    $ cabal sandbox add-source /path/to/ARSim
    $ cabal install --dependencies only
    $ cabal build

## Instructions

Examples are located in the [Examples](Examples) branch of the package and
includes a stand-alone ABS example (Johan Nordlander), a Simulink driven ABS
example and a Simulink driven ACC example (Oskar Abrahamsson).

The examples are parametric in their task assignment. The [AUTOSAR](AUTOSAR.hs) 
module exports a few of these examples. 

All SWC source coude is located in [AUTOSAR](AUTOSAR). Shared SWC code is 
located in [AUTOSAR/Shared](AUTOSAR/Shared).
