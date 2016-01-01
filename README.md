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

## Useful documents
### Gauche VM

  - http://practical-scheme.net/docs/stack-j.html
  - http://practical-scheme.net/gauche/memo-j.html
  - http://practical-scheme.net/gauche/memo-stack-j.html
  - http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AVMの最適化

### Transition from 1st gen to 2nd gen GVM:

  - http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AVMの最適化%3AFor%200.8.4

### Restructuring stack frame (for 1st gen GVM):

  - http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AVMの最適化%3AFor%200.6.3

###  The source of the original hoc can be found in

  -  http://www.cs.princeton.edu/~bwk/btl.mirror/new/

