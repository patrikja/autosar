# Revision history for accdemo

This model will initially act as a cruise-control sandbox. A cruise control
system in its simplest form is basically just a control circuit which regulates
the vehicle throttle based on the signed difference between desired cruise 
velocity and actual vehicle velocity.

## 0.1.0.0  -- 2016-06-13

* Implemented a **simple** cruise control in the AUTOSAR DSL. The code is
  somewhat unstructured, but includes a simple PID controller, a simulated
  cruise control ECU (making use of the PID) and a dummy car.

A Simulink car with an engine is not yet in place, but the system manages to
regulate the speed of an very basically modelled Haskell/AUTOSAR DSL car.
Emphasis seems to be on the integration scale parameter in order to smoothly
control the vehicle speed using the throttle.
