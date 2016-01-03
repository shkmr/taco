# IKA and TACO

## IKA -- Assembler

* IKA is an assembler for Gauche VM.
  Assembled code can be executed by vm-code-execute! (in vmhack module).

* vmhack directory contains extlib for vm-code-execute!


## TACO -- Compiler

* TACO3 is TACO2 retargeted to current Gauche VM (from 0.8.4 and on).

* TACO2 is TACO1 + Simple optimization.

* TACO1 is something similar to hoc6 describied in
  ``The UNIX Programming Environment'' by Kernighan and Pike,
  implemented in Gauche and the target VM is MGVM.

*  MGVM (Mock-GVM) is a subset of the first generation Gauche VM
  (corresponding to Gauche version upto 0.8.3), implemented in Gauche
  (to study stack frame of Gauche VM at the time).

###  The source of the original hoc can be found in

  -  http://www.cs.princeton.edu/~bwk/btl.mirror/new/

