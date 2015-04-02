:- op(901, xfy, ':').
:- op(902, xfx, '==>').
:- op(905, xfx, '---').
:- op(906, xfx, '--->').


%%%%% Combining reductions

[]  --- hear(_A,_L) --->  []   .

% [ P | Ps ]  --- say(A,L) --->   [ P2 | Ps2 ]  :-   P  --- say(A,L) --->   P2  ,  Ps  --- hear(A,L) --->  Ps2 .

[ P | Ps ]  --- say(A,L) --->   Rs            :-   P  --- say(A,L) --->   P2  ,  Ps  --- hear(A,L) --->  Ps2 ,
                                                 flateval(P2, Ps2, Rs).
[ P | Ps ]  --- say(A,L) --->   [ P2 | Ps2 ]  :-   P  --- hear(A,L) --->  P2  ,  Ps  --- say(A,L) --->   Ps2 .
[ P | Ps ]  --- hear(A,L) --->  [ P2 | Ps2 ]  :-   P  --- hear(A,L) --->  P2  ,  Ps  --- hear(A,L) --->  Ps2 .

[ P | Ps ]  --- say(A,L) --->   [ P  | Ps2 ]  :-   Ps  --- say(A,L) --->   Ps2  ,  ignore((A,L),P)    .
[ P | Ps ]  --- hear(A,L) --->  [ P  | Ps2 ]  :-   Ps  --- hear(A,L) --->  Ps2  ,  ignore((A,L),P)    .
% Think of   ignore((A,L),P) :- P--- hear2(A,L) --->P.  But implemented to avoid overlap.

[]  --- delta(_T) --->  []    .
[ P | Ps ]  --- delta(T) --->   [ P2 | Ps2 ]  :-   P  --- delta(T) --->   P2  ,    Ps  --- delta(T) --->  Ps2  .


%%%%% Exclusive areas

% Strict stack-based locking enforced: reduction gets stuck otherwise
%   (TODO: comment about flagging error on attempted "bad" rte_exit?)
rinst(R:I, C, Xs, rte_enter(X,K))        --- say(X:I,enter) --->     rinst(R:I, C, [X|Xs], ap(K,ok))  .
rinst(R:I, C, [X|Xs], rte_exit(X,K))     --- say(X:I,exit) --->      rinst(R:I, C, Xs, ap(K,ok))      .
excl(X:I, free)                          --- hear(X:I,enter) --->    excl(X:I, taken)                    .
excl(X:I, taken)                         --- hear(X:I,exit) --->     excl(X:I, free)                     .

%%%%% Inter-runnable variables

rinst(R:I, C, Xs, rte_irv_read(S,K))     --- say(S:I,irvr(V)) --->     rinst(R:I, C, Xs, ap(K,V))  .
rinst(R:I, C, Xs, rte_irv_write(S,K))    --- say(S:I,irvw(_V)) --->    rinst(R:I, C, Xs, ap(K,ok)) .
irv(S:I, V)                              --- hear(S:I,irvr(V)) --->    irv(S:I, V)                    .
irv(S:I, _)                              --- hear(S:I,irvw(V)) --->    irv(S:I, V)                    .

%%%%% Sending/receiving

rinst(R:I, C, Xs, rte_receive(E:P,K))    --- say(E:P:I,rcv(V)) --->        rinst(R:I, C, Xs, ap(K,V)) .
rinst(R:I, C, Xs, rte_send(E:P,V,K))     --- say(E:P:I,snd(V,Res)) --->    rinst(R:I, C, Xs, ap(K,Res)).

qelem(E:P:I, N, [V|Vs])                  --- hear(E:P:I,rcv(V)) --->       qelem(E:P:I, N, Vs)    .
qelem(E:P:I, N, [])                      --- hear(E:P:I,rcv(no_data)) ---> qelem(E:P:I, N, [])    .
qelem(E:P:I, N, Vs)                      --- hear(A,snd(V,ok)) --->        qelem(E:P:I, N, Vs1)
    :-    A==>E:P:I, length(Vs,X), X < N, append(Vs,[V],Vs1)    .
qelem(E:P:I, N, Vs)                      --- hear(A,snd(_V,limit)) --->    qelem(E:P:I, N, Vs)
    :-    A==>E:P:I, length(Vs,N)    .
qelem(E:P:I, N, Vs)                      --- hear(A,snd(V,Res)) --->       qelem(E:P:I, N, Vs1)
    :-    A==>E:P:I, length(Vs,X), X < N, append(Vs,[V],Vs1), Res \= ok    .

% data_received(E:P) is a static property of of this runnable
%   from pending it will move on to spawn an rinst which will execute some "data handler" code
runnable(R:I, K, T, _, N)                --- hear(A,snd(_V,ok)) --->   runnable(R:I, K, T, pending, N)
    :-    A==>E:P:I, events(R:I, data_received(E:P))    .
runnable(R:I, K, T, Act, N)              --- hear(A,snd(_V,limit)) --->runnable(R:I, K, T, Act, N)
    :-    A==>E:P:I, events(R:I, data_received(E:P))    .

%%%%% Reading/writing (unbuffered versions of rcv and snd)

rinst(R:I, C, XS, rte_read(E:P,K))       --- say(E:P:I,rd(V)) --->     rinst(R:I, C, XS, ap(K,V)) .
rinst(R:I, C, XS, rte_write(E:P,V,K))    --- say(E:P:I,wr(V)) --->     rinst(R:I, C, XS, ap(K,ok)).
delem(E:P:I, _U, V)                      --- hear(E:P:I,rd(V)) --->    delem(E:P:I, false, V)        .
delem(E:P:I, _U, _)                      --- hear(A,wr(V)) --->        delem(E:P:I, true, V)
    :-    A==>E:P:I .
runnable(R:I, K, T, _, N)                --- hear(A,wr(_V)) --->       runnable(R:I, K, T, pending, N)
    :-    A==>E:P:I, events(R:I, data_received(E:P))    .
rinst(R:I, C, XS, rte_is_updated(E:P,K)) --- say(E:P:I,up(U)) --->     rinst(R:I, C, XS, ap(K,U)) .
rinst(R:I, C, XS, rte_invalidate(E:P,K)) --- say(E:P:I,inv) --->       rinst(R:I, C, XS, ap(K,ok)).
delem(E:P:I, U, V)                       --- hear(E:P:I,up(U)) --->    delem(E:P:I, U, V)            .
delem(E:P:I, _U, _)                      --- hear(A,inv) --->          delem(E:P:I, true, invalid)
    :-    A==>E:P:I .

%%%%% Calling a server

rinst(R:I, C, XS, rte_call(O:P,V,K))     --- say(O:P:I,call(V,O:P:I,Res)) --->  rinst(R:I, C, XS, ap(K,Res))
    :- serverCallPoint(R:I, async(O:P)) ; Res \= ok   .
rinst(R:I, C, XS, rte_call(O:P,V,K))     --- say(O:P:I,call(V,O:P:I,ok)) --->   rinst(R:I, C, XS, rte_result(O:P,K))
    :- serverCallPoint(R:I, sync(O:P))  .
runnable(R:I, K, T, serving(Cs,Vs), N)   --- hear(A,call(V,C,ok)) --->          runnable(R:I, K, T, serving(Cs1,Vs1), N)
    :-
    A==>O:P:I, events(R:I, operationInvoked(O:P)),
    \+ member(C,Cs), append(Cs,[C],Cs1), append(Vs,[V],Vs1)
    .
runnable(R:I, K, T, serving(Cs,Vs), N)   --- hear(A,call(_V,C,limit)) --->      runnable(R:I, K, T, serving(Cs,Vs), N)
    :-
    A==>O:P:I, events(R:I, operationInvoked(O:P)),
    member(C, Cs)
    .

%%%%% Obtaining a server result

rinst(R:I, C, XS, rte_result(O:P,K))  --- say(O:P:I,res(V)) --->        rinst(R:I, C, XS, ap(K,V))  .
rinst(A, O:P:I, [], return(V))        --- say(O:P:I,ret(V)) --->        rinst(A, nil, [], return(void)).
opres(O:P:I, [V|Vs])                  --- hear(O:P:I,res(V)) --->       opres(O:P:I, Vs)               .
opres(O:P:I, [])                      --- hear(O:P:I,res(no_data)) ---> opres(O:P:I, [])
    :-    async_result(O:P:I)   .
opres(O:P:I, Vs)                      --- hear(O:P:I,ret(V)) --->       opres(O:P:I, Vs1)
    :-    append(Vs,[V],Vs1)    .

%%%%% Spawning and terminating

rinst(A, nil, [], return(_V))            --- say(A,term) --->     []    .
runnable(A, K, T, Act, N)                --- hear(A,term) --->    runnable(A, K, T, Act, N1)  :- N1 is N-1  .
% Note the "time left" state has to be zero for these rules to fire.
runnable(A, K, 0, pending, N)            --- say(A,new) --->    [ runnable(A, K, T, idle, N1)
                                                                , rinst(A, nil, [], ap(K,void)) ]
    :-
    (N == 0 ; canBeInvokedConcurrently(A)),
    minimumStartInterval(A, T),
    N1 is N+1
    .
runnable(A, K, 0, serving([C|Cs],[V|Vs]), N) --- say(A,new) --->   [ runnable(A, K, T, serving(Cs,Vs), N1)
                                                                   , rinst(A, C, [], ap(K,V)) ]
    :-
    (N == 0 ; canBeInvokedConcurrently(A)),
    minimumStartInterval(A, T),
    N1 is N+1
    .

%%%%% Passing time

timer(A, 0)                 --- say(A,tick) --->  timer(A, T)                :-    events(A, timing(T))    .
runnable(A, K, T, _, N)     --- hear(A,tick) ---> runnable(A, K, T, pending, N) .

% The "time left" state V is always decreasing in delta(T) steps, and never negative.
timer(A, V)                 --- delta(T) --->     timer(A, V1)               :-    V >= T, V1 is V-T    .
runnable(A, K, V, Act, N)   --- delta(T) --->     runnable(A, K, V1, Act, N) :-    V >= T, V1 is V-T    .
runnable(A, K, 0, Act, N)   --- delta(_T) --->    runnable(A, K, 0, Act, N)  .
rinst(A, C, XS, Code)       --- delta(_T) --->    rinst(A, C, XS, Code)      .
excl(A, V)                  --- delta(_T) --->    excl(A, V)                 .
irv(A, V)                   --- delta(_T) --->    irv(A, V)                  .
qelem(A, N, Vs)             --- delta(_T) --->    qelem(A, N, Vs)            .
delem(A, U, V)              --- delta(_T) --->    delem(A, U, V)             .
opres(A, Vs)                --- delta(_T) --->    opres(A, Vs)               .


%%%%% Ignoring broadcasts

ignore(_AL, rinst(_B,_C,_Xs,_K))   .
ignore(_AL, timer(_B,_T))             .

ignore((A,_L), runnable(B, _K, _T, _Act, _N))   :- A \== B.
ignore((A,_L), excl(B,_V))                      :- A \== B.
ignore((A,_L), irv(B,_V))                       :- A \== B.
ignore((A,_L), qelem(B,_N,_Vs))                 :- A \== B, \+ A==>B.
ignore((A,_L), delem(B,_U,_V))                  :- A \== B, \+ A==>B.
ignore((A,_L), opres(B,_Vs))                    :- A \== B, \+ A==>B.


%%%%%  Helper predicate: flattening and evaluating reduction results

flateval([],              Q, Q)  :- !  .
flateval([ M | P ],       Q, R)  :- !, flateval(P, Q, R1),  flateval(M, R1, R)  .
flateval(rinst(A,C,Xs,M), Q, [ rinst(A,C,Xs,N) | Q ])  :- !, eval(M, N)   .
flateval(M,               Q, [ M               | Q ]).

% Experiment with "treelike" process soup
treeeval([],              [])         :- !  .
treeeval([ M | P ],       [M2 | P2])  :- !, treeeval(M, M2), treeeval(P, P2).
treeeval(rinst(A,C,Xs,M), [ rinst(A,C,Xs,N) | _Q ])  :- !, eval(M, N)   .
treeeval(M,               [ M               | _Q ]).

%%%%% Helper predicate: evaluating terms

eval(V, V)              :-    var(V), !    .
eval(ap(T,V), ap(T,V))  :-    var(T), !    .
eval(ap(F,E), R)        :-    eval(F, fn(X,T)), !,    eval(E, V),    X = V,    eval(T, R)    .
eval(ap(T,_V), R)       :- !, eval(T, R)   .
eval(fn(X,T), fn(X,T))  :- ! .
eval(if(E,A,_B), R)     :-    E,    !, eval(A, R)    .
eval(if(E,_A,B), R)     :-    \+ E, !, eval(B, R)    .
eval((A,B), (A1,B1))    :- !, eval(A, A1),     eval(B, B1)    .
eval([A|B], [A1|B1])    :- !, eval(A, A1),     eval(B, B1)    .
eval((A:B), (A1:B1))    :- !, eval(A, A1),     eval(B, B1)    .
eval(E, R)              :-    E =.. [H|As], funWithArgsToEvaluate(H), !, eval(As, Bs), R =.. [H|Bs].
eval(V, V)              :-    atom(V), !  .
eval(E, R)              :-    R is E      .

funWithArgsToEvaluate(H):- name(H,N), (append('rte_',_,N) ; N = 'return').
