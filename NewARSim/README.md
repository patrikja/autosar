# NewARSim

(Something here)

## Task assignments

The DSL has been extended with support for manual task mappings. Task mappings
are performed like so:

```haskell
mySWC :: AUTOSAR MySWC
mySWC = atomic $ do
  ... 
  -- A runnable w/o task assignment:
  runnable myInvocation myEvents myRteCode
 
  -- A runnable with a regular task assignment, with privilege @p@:
  runnableT ["my_task" :-> p] myInvocation2 myEvents2 myRteCode2

  -- A runnable with a task assignment with privilege @p@, that runs
  -- every @n@:th activation of the task:
  runnableT ["my_task" :>> (p, n)] myInvocation3 myEvents3 myRteCode3

  ...
```

Each task that appears in a task assignment needs a declaration which specifies
what event triggers it somewhere in the program:

```haskell
mySystem :: AUTOSAR MySystem
mySystem = composition $ do
  ...
  declareTask "my_task" (TimingEvent (t :: Double))
  declareTask "foo"     (DataReceivedEvent (bar :: DataElem q a r)
  ...
```

Unless there is a one-to-one correspondence the simulator will exit with an
exception during initialization. Incorrect task assignments will be revealed in
the trace during simulation (and can be seen either by calling `printAll` on the
trace or in stdout when running external simulation).

