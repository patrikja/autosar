:- op(901, xfy, ':').
:- op(905, xfx, '---').
:- op(906, xfx, '--->').

:- consult('semantics.pl').

example1(L,R) :- 
  Proc1 = rinst(i:r1,nil,[],rte_enter(x,Cont1)),
  Proc2 = rinst(i:r2,nil,[],rte_enter(x,Cont2)),
  Block = [excl(i:x,free), Proc1, Proc2],
  [excl(i:z,free) | Block] ---L---> R.


badexample(L,R) :- 
  Proc1 = rinst(i:r1,nil,[],rte_enter(x,Cont1)),
  Proc2 = rinst(i:r2,nil,[],rte_enter(x,Cont2)),
  Block = [excl(i:x,free), Proc1, Proc2],
  [Block | excl(i:z,free)] ---L---> R.

