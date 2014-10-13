:- use_module(library(lists)).

:- op(901, xfy, ':').
:- op(905, xfx, '---').
:- op(906, xfx, '--->').


%%%%% Combining reductions

[]  ---hear(A,L)--->  []
    .
[ P | Q ]  ---say(A,L)--->  R
    :-
    P  ---say(A,L)--->  P2
    ,
    Q  ---hear(A,L)--->  Q2
    ,
    flateval(P2, Q2, R)                 % Intuitively the same as R = [ P2 | Q2 ]
    .
[ P | Q ]  ---say(A,L)--->  [ P2 | Q2 ]
    :-
    P  ---hear(A,L)--->  P2
    ,
    Q  ---say(A,L)--->  Q2
    .
[ P | Q ]  ---hear(A,L)--->  [ P2 | Q2 ]
    :-
    P  ---hear(A,L)--->  P2
    ,
    Q  ---hear(A,L)--->  Q2
    .
[ P | Q ]  ---say(A,L)--->  [ P | Q2 ]
    :-
    Q  ---say(A,L)--->  Q2 
    ,
    tolerate(A,P)
    .
[ P | Q ]  ---hear(A,L)--->  [ P | Q2 ]
    :-
    Q  ---hear(A,L)--->  Q2
    ,
    tolerate(A,P)
    .
[]  ---delta(T)--->  []
    .
[ P | Q ]  ---delta(T)--->  [ P2 | Q2 ]
    :-
    P  ---delta(T)--->  P2
    ,
    Q  ---delta(T)--->  Q2
    .


%%%%% Tolerating broadcasts

tolerate(A, rinst(B,C,Xs,Cont))     .
tolerate(A, timer(B,T))             .

tolerate(A, runnable(B, T, Act, N)) :- A \== B.
tolerate(A, excl(B,V))              :- A \== B.
tolerate(A, irv(B,V))               :- A \== B.
tolerate(A, qelem(B,N,Vs))          :- A \== B, not connected(A,B).
tolerate(A, delem(B,U,V))           :- A \== B, not connected(A,B).
tolerate(A, opres(B,Vs))            :- A \== B, not connected(A,B).


%%%%% Exclusive areas

rinst(I:R, C, Xs, rte_enter(X,Cont))   ---say(I:X,enter)--->     rinst(I:R, C, X.Xs, ap(Cont,ok))
    .
rinst(I:R, C, X.Xs, rte_exit(X,Cont))  ---say(I:X,exit)--->      rinst(I:R, C, Xs, ap(Cont,ok))
    .
excl(I:X, free)                        ---hear(I:X,enter)--->     excl(I:X, taken)
    .
excl(I:X, taken)                       ---hear(I:X,exit)--->      excl(I:X, free)
    .

%%%%% Inter-runnable variables

rinst(I:R, C, Xs, rte_irv_read(S,Cont))     ---say(I:S,irvr(V))--->     rinst(I:R, C, Xs, ap(Cont,V))
    .
rinst(I:R, C, Xs, rte_irv_write(S, Cont))   ---say(I:S,irvw(V))--->     rinst(I:R, C, Xs, ap(Cont,ok))
    .
irv(I:S, V)                                 ---hear(I:S,irvr(V))--->    irv(I:S, V)
    .
irv(I:S, _)                                 ---hear(I:S,irvw(V))--->    irv(I:S, V)
    .

%%%%% Sending/receiving

rinst(I:R, C, Xs, rte_receive(P:E,Cont))    ---say(I:P:E,rcv(V))--->            rinst(I:R, C, Xs, ap(Cont,V))
    .
rinst(I:R, C, Xs, rte_send(P:E,V,Cont))     ---say(I:P:E,snd(V,Res))--->        rinst(I:R, C, Xs, ap(Cont,Res))
    .
qelem(I:P:E, N, V.Vs)                       ---hear(I:P:E,rcv(V))--->           qelem(I:P:E, N, Vs)
    .
qelem(I:P:E, N, [])                         ---hear(I:P:E,rcv(no_data))--->     qelem(I:P:E, N, [])
    .
qelem(I:P:E, N, Vs)                         ---hear(A,snd(V,ok))--->            qelem(I:P:E, N, Vs1)
    :-
    connected(A, I:P:E), length(Vs,X), X < N, append(Vs,[V],Vs1)
    .
qelem(I:P:E, N, Vs)                         ---hear(A,snd(V,limit))--->         qelem(I:P:E, N, Vs)
    :-
    connected(A, I:P:E), length(Vs,N)
    .
qelem(I:P:E, N, Vs)                         ---hear(A,snd(V,Res))--->           qelem(I:P:E, N, Vs1)
    :-
    connected(A, I:P:E), length(VS,X), X < N, append(Vs,[V],Vs1), Res \= ok
    .
runnable(I:R, T, _, N)                      ---hear(I:P:E,snd(V,ok))--->        runnable(I:R, T, pending, N)
    :-
    events(I:R, data_received(P:E))
    .
runnable(I:R, T, Act, N)                    ---hear(I:P:E,snd(V,limit))--->     runnable(I:R, T, Act, N)
    :-
    events(I:R, data_received(P:E))
    .

%%%%% Reading/writing

rinst(I:R, C, XS, rte_read(P:E,Cont))           ---say(I:P:E,rd(V))--->     rinst(I:R, C, XS, ap(Cont,V))
    .
rinst(I:R, C, XS, rte_write(P:E,V,Cont))        ---say(I:P:E,wr(V))--->     rinst(I:R, C, XS, ap(Cont,ok))
    .
delem(I:P:E, U, V)                              ---hear(I:P:E,rd(V))--->    delem(I:P:E, false, V)
    .
delem(I:P:E, U, _)                              ---hear(A,wr(V))--->        delem(I:P:E, true, V)
    :-
    connected(A, I:P:E)
    .
runnable(I:R, T, _, N)                          ---hear(A,wr(V))--->        runnable(I:R, T, pending, N)
    :-
    connected(A, I:P:E), events(I:R, data_received(P:E))
    .
rinst(I:R, C, XS, rte_is_updated(P:E,Cont))     ---say(I:P:E,up(U))--->     rinst(I:R, C, XS, ap(Cont,U))
    .
rinst(I:R, C, XS, rte_invalidate(P:E,Cont))     ---say(I:P:E,inv)--->       rinst(I:R, C, XS, ap(Cont,ok))
    .
delem(I:P:E, U, V)                              ---hear(I:P:E,up(U))--->    delem(I:P:E, U, V)
    .
delem(I:P:E, U, _)                              ---hear(A,inv)--->          delem(I:P:E, true, invalid)
    :-
    connected(A, I:P:E)
    .

%%%%% Calling a server

rinst(I:R, C, XS, rte_call(P:O,V,Cont))     ---say(I:P:O,call(V,I:P:O,Res))--->     rinst(I:R, C, XS, ap(Cont,Res))
    :-
    server_call_point(I:R, async(P:O)),
    Res \= ok
    .
rinst(I:R, C, XS, rte_call(P:O,V,Cont))     ---say(I:P:O,call(V,I:P:O,ok))--->      rinst(I:R, C, XS, rte_result(P:O,Cont))
    :-
    server_call_point(I:R, sync(P:O))
    .
runnable(I:R, T, serving(Cs,Vs), N)         ---hear(A,call(V,C,ok))--->             runnable(I:R, T, serving(Cs1,Vs1), N)
    :-
    connected(A, I:P:O), events(I:R, op_invoked(P:O)),
    not member(C,Cs), append(Cs,[C],Cs1), append(Vs,[V],Vs1)
    .
runnable(I:R, T, serving(Cs,Vs), N)         ---hear(A,call(V,C,limit))--->          runnable(I:R, T, serving(Cs,Vs), N)
    :-
    connected(A, I:P:O), events(I:R, op_invoked(P:O)),
    member(C, Cs)
    .

%%%%% Obtaining a server result

rinst(I:R, C, XS, rte_result(P:O,Cont))     ---say(I:P:O,res(V))--->            rinst(I:R, C, XS, ap(Cont,V))
    .
rinst(A, I:P:O, [], rte_terminate(V))       ---say(I:P:O,ret(V))--->            rinst(A, -, [], rte_terminate(void))
    .
opres(I:P:O, V.Vs)                          ---hear(I:P:O,res(V))--->           opres(I:P:O, Vs)
    .
opres(I:P:O, [])                            ---hear(I:P:O,res(no_data))--->     opres(I:P:O, [])
    :-
    async_result(I:P:O)
    .
opres(I:P:O, Vs)                            ---hear(I:P:O,ret(V))--->           opres(I:P:O, Vs1)
    :-
    append(Vs,[V],Vs1)
    .

%%%%% Spawning and terminating

rinst(A, -, [], rte_terminate(V))        ---say(A,term)--->     []
    .
runnable(A, T, Act, N)                   ---hear(A,term)--->    runnable(A, T, Act, N1)
    :-
    N1 is N-1
    .
runnable(A, 0, pending, N)               ---say(A,new)--->  [ runnable(A, T, idle, N1), rinst(A, -, [], ap(Cont,void)) ]
    :-
    (N == 0 ; can_be_invoked_concurrently(A)),
    minimum_start_interval(A, T),
    implementation(A, Cont),
    N1 is N+1
    .
runnable(A, 0, serving(C.Cs,V.Vs), N)    ---say(A,new)--->  [ runnable(A, T, serving(Cs,Vs), N1), rinst(A, C, [], ap(Cont,V)) ]
    :-
    (N == 0 ; can_be_invoked_concurrently(A)),
    minimum_start_interval(A, T), 
    implementation(A, Cont),
    N1 is N+1
    .

%%%%% Passing time

timer(A, 0)             ---say(A,tick)--->  timer(A, T)
    :-
    events(A, timing(T))
    .
runnable(A, T, _, N)    ---hear(A,tick)---> runnable(A, T, pending, N)
    .

runnable(A, V, Act, N)  ---delta(T)--->     runnable(A, V1, Act, N)
    :-
    V >= T, V1 is V-T
    .
timer(A, V)             ---delta(T)--->     timer(A, V1)
    :-
    V >= T, V1 is V-T
    .
rinst(A, C, XS, Code)   ---delta(T)--->     rinst(A, C, XS, Code)
    .
excl(A, V)              ---delta(T)--->     excl(A, V)
    .
irv(A, V)               ---delta(T)--->     irv(A, V)
    .
qelem(A, N, Vs)         ---delta(T)--->     qelem(A, N, Vs)
    .
delem(A, U, V)          ---delta(T)--->     delem(A, U, V)
    .
opres(A, Vs)            ---delta(T)--->     opres(A, Vs)
    .


%%%%%  Helper predicate: flattening and evaluating reduction results

flateval([], Q, Q)
    :- ! 
    .
flateval([ M | P ], Q, R)
    :- !,
    flateval(P, Q, R1),
    flateval(M, R1, R)
    .
flateval(rinst(A,C,Xs,M), Q, [ rinst(A,C,Xs,N) | Q ])
    :- !,
    eval(M, N)
    .
flateval(M, Q, [M|Q])
    .


%%%%% Helper predicate: evaluating terms

eval(V, V)
    :-
    var(V), !
    .
eval(ap(T,V), ap(T,V))
    :-
    var(T), !
    .
eval(ap(F,E), R)
    :-
    eval(F, fn(X,T)), !,
    eval(E, V),
    X = V,
    eval(T, R)
    .
eval(ap(T,V), R)
    :- !,
    eval(T, R)
    .
eval(fn(X,T), fn(X,T))
    :- !
    .
eval(if(E,A,B), R)
    :-
    E, !,
    eval(A, R)
    .
eval(if(E,A,B), R)
    :-
    not E, !,
    eval(B, R)
    .
eval((A,B), (A1,B1))
    :- !,
    eval(A, A1), 
    eval(B, B1)
    .
eval((A.B), (A1.B1))
    :- !,
    eval(A, A1), 
    eval(B, B1)
    .
eval((A:B), (A1:B1))
    :- !,
    eval(A, A1), 
    eval(B, B1)
    .
eval(E, R)
    :-
    E =.. [H|A], name(H,N), append("rte_",_,N), !,
    eval(A, B),
    R =.. [H|B]
    .
eval(V, V)
    :-
    atom(V), !
    .
eval(E, R)
    :-
    R is E
    .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Static info examples %%%%%%%%%%%%%%%%%%%%%%%%%%%%

connected(i:p0:e, i:p3:e).
connected(_, _) :- false.

event(i:r1, timing(300)).
event(i:r1, data_received(p3:e)).

implementation(i:r1, rte_terminate(void)).

minimum_start_interval(i:r1, 10).

can_be_invoked_concurrently(i:r2).

server_call_point(i:r1, sync(p2:o)).
