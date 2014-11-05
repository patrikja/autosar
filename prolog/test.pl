:- op(901, xfy, ':').
:- op(905, xfx, '---').
:- op(906, xfx, '--->').

:- consult('semantics.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Static info examples %%%%%%%%%%%%%%%%%%%%%%%%%%%%

connected(i:p0:e, i:p3:e).
connected(_, _) :- false.

event(i:r1, timing(300)).
event(i:r1, data_received(p3:e)).

implementation(i:r1, return(void)).

minimum_start_interval(i:r1, 10).

can_be_invoked_concurrently(i:r2).

server_call_point(i:r1, sync(p2:o)).

%%%%%%%%%%%%%%%%

example1(L,R) :- 
%  Cont1 = fn(X,rte_exit(x, Cont2)),
%  Cont2 = fn(X,rte_exit(x, fn(Y,return(void)))),
  Proc1 = rinst(i:r1,nil,[],rte_enter(x,Cont1)),
  Proc2 = rinst(i:r2,nil,[],rte_enter(x,Cont2)),
  Block = [excl(i:x,free), Proc1, Proc2],
  [[excl(i:z,free), []] | Block] ---L---> R.


badexample(L,R) :- 
  Proc1 = rinst(i:r1,nil,[],rte_enter(x,Cont1)),
  Proc2 = rinst(i:r2,nil,[],rte_enter(x,Cont2)),
  Block = [excl(i:x,free), Proc1, Proc2],
  [Block | excl(i:z,free)] ---L---> R.

