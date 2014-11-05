:- op(901, xfy, ':').
:- op(905, xfx, '---').
:- op(906, xfx, '--->').


%%%%% Combining reductions

[]  ---hear(_A,_L)--->  []   .

% [ P | Ps ]  ---say(A,L)--->   [ P2 | Ps2 ]  :-   P  ---say(A,L)--->   P2  ,  Ps  ---hear(A,L)--->  Ps2 .

[ P | Ps ]  ---say(A,L)--->   Rs            :-   P  ---say(A,L)--->   P2  ,  Ps  ---hear(A,L)--->  Ps2 ,
                                                 flateval(P2, Ps2, Rs).
[ P | Ps ]  ---say(A,L)--->   [ P2 | Ps2 ]  :-   P  ---hear(A,L)--->  P2  ,  Ps  ---say(A,L)--->   Ps2 .
[ P | Ps ]  ---hear(A,L)--->  [ P2 | Ps2 ]  :-   P  ---hear(A,L)--->  P2  ,  Ps  ---hear(A,L)--->  Ps2 .

[ P | Ps ]  ---say(A,L)--->   [ P  | Ps2 ]  :-   Ps  ---say(A,L)--->   Ps2  ,  ignore((A,L),P)    .
[ P | Ps ]  ---hear(A,L)--->  [ P  | Ps2 ]  :-   Ps  ---hear(A,L)--->  Ps2  ,  ignore((A,L),P)    .
% Think of   ignore((A,L),P) :- P---hear2(A,L)--->P.  But implemented to avoid overlap.

[]  ---delta(_T)--->  []    .
[ P | Ps ]  ---delta(T)--->   [ P2 | Ps2 ]  :-   P  ---delta(T)--->   P2  ,    Ps  ---delta(T)--->  Ps2  .


%%%%% Exclusive areas

% Strict stack-based locking enforced: reduction gets stuck otherwise 
%   (TODO: comment about flagging error on attempted "bad" rte_exit?)
rinst(I:R, C, Xs, rte_enter(X,Cont))     ---say(I:X,enter)--->     rinst(I:R, C, [X|Xs], ap(Cont,ok))  .
rinst(I:R, C, [X|Xs], rte_exit(X,Cont))  ---say(I:X,exit)--->      rinst(I:R, C, Xs, ap(Cont,ok))      .
excl(I:X, free)                          ---hear(I:X,enter)--->    excl(I:X, taken)                    .
excl(I:X, taken)                         ---hear(I:X,exit)--->     excl(I:X, free)                     .

%%%%% Inter-runnable variables

rinst(I:R, C, Xs, rte_irv_read(S,Cont))     ---say(I:S,irvr(V))--->     rinst(I:R, C, Xs, ap(Cont,V))  .
rinst(I:R, C, Xs, rte_irv_write(S,Cont))    ---say(I:S,irvw(_V))--->    rinst(I:R, C, Xs, ap(Cont,ok)) .
irv(I:S, V)                                 ---hear(I:S,irvr(V))--->    irv(I:S, V)                    .
irv(I:S, _)                                 ---hear(I:S,irvw(V))--->    irv(I:S, V)                    .

%%%%% Sending/receiving

rinst(I:R, C, Xs, rte_receive(P:E,Cont)) ---say(I:P:E,rcv(V))--->        rinst(I:R, C, Xs, ap(Cont,V)) .
rinst(I:R, C, Xs, rte_send(P:E,V,Cont))  ---say(I:P:E,snd(V,Res))--->    rinst(I:R, C, Xs, ap(Cont,Res)).

qelem(I:P:E, N, [V|Vs])                  ---hear(I:P:E,rcv(V))--->       qelem(I:P:E, N, Vs)    .
qelem(I:P:E, N, [])                      ---hear(I:P:E,rcv(no_data))---> qelem(I:P:E, N, [])    .
qelem(I:P:E, N, Vs)                      ---hear(A,snd(V,ok))--->        qelem(I:P:E, N, Vs1)
    :-    connected(A, I:P:E), length(Vs,X), X < N, append(Vs,[V],Vs1)    .
qelem(I:P:E, N, Vs)                      ---hear(A,snd(_V,limit))--->    qelem(I:P:E, N, Vs)
    :-    connected(A, I:P:E), length(Vs,N)    .
qelem(I:P:E, N, Vs)                      ---hear(A,snd(V,Res))--->       qelem(I:P:E, N, Vs1)
    :-    connected(A, I:P:E), length(Vs,X), X < N, append(Vs,[V],Vs1), Res \= ok    .

% data_received(P:E) is a static property of of this runnable
%   from pending it will move on to spawn an rinst which will execute some "data handler" code
runnable(I:R, T, _, N)                   ---hear(A,snd(_V,ok))--->   runnable(I:R, T, pending, N)
    :-    connected(A, I:P:E), events(I:R, data_received(P:E))    .
runnable(I:R, T, Act, N)                 ---hear(A,snd(_V,limit))--->runnable(I:R, T, Act, N)
    :-    connected(A, I:P:E), events(I:R, data_received(P:E))    .

%%%%% Reading/writing (unbuffered versions of rcv and snd)

rinst(I:R, C, XS, rte_read(P:E,Cont))        ---say(I:P:E,rd(V))--->     rinst(I:R, C, XS, ap(Cont,V)) .
rinst(I:R, C, XS, rte_write(P:E,V,Cont))     ---say(I:P:E,wr(V))--->     rinst(I:R, C, XS, ap(Cont,ok)).
delem(I:P:E, _U, V)                          ---hear(I:P:E,rd(V))--->    delem(I:P:E, false, V)        .
delem(I:P:E, _U, _)                          ---hear(A,wr(V))--->        delem(I:P:E, true, V)
    :-    connected(A, I:P:E)    .
runnable(I:R, T, _, N)                       ---hear(A,wr(_V))--->       runnable(I:R, T, pending, N)
    :-    connected(A, I:P:E), events(I:R, data_received(P:E))    .
rinst(I:R, C, XS, rte_is_updated(P:E,Cont))  ---say(I:P:E,up(U))--->     rinst(I:R, C, XS, ap(Cont,U)) .
rinst(I:R, C, XS, rte_invalidate(P:E,Cont))  ---say(I:P:E,inv)--->       rinst(I:R, C, XS, ap(Cont,ok)).
delem(I:P:E, U, V)                           ---hear(I:P:E,up(U))--->    delem(I:P:E, U, V)            .
delem(I:P:E, _U, _)                          ---hear(A,inv)--->          delem(I:P:E, true, invalid)
    :-    connected(A, I:P:E)    .

%%%%% Calling a server

rinst(I:R, C, XS, rte_call(P:O,V,Cont))  ---say(I:P:O,call(V,I:P:O,Res))--->  rinst(I:R, C, XS, ap(Cont,Res))
    :- server_call_point(I:R, async(P:O)) ; Res \= ok   .
rinst(I:R, C, XS, rte_call(P:O,V,Cont))  ---say(I:P:O,call(V,I:P:O,ok))--->   rinst(I:R, C, XS, rte_result(P:O,Cont))
    :- server_call_point(I:R, sync(P:O))  .
runnable(I:R, T, serving(Cs,Vs), N)      ---hear(A,call(V,C,ok))--->          runnable(I:R, T, serving(Cs1,Vs1), N)
    :-
    connected(A, I:P:O), events(I:R, op_invoked(P:O)),
    \+ member(C,Cs), append(Cs,[C],Cs1), append(Vs,[V],Vs1)
    .
runnable(I:R, T, serving(Cs,Vs), N)      ---hear(A,call(_V,C,limit))--->      runnable(I:R, T, serving(Cs,Vs), N)
    :-
    connected(A, I:P:O), events(I:R, op_invoked(P:O)),
    member(C, Cs)
    .

%%%%% Obtaining a server result

rinst(I:R, C, XS, rte_result(P:O,Cont))  ---say(I:P:O,res(V))--->        rinst(I:R, C, XS, ap(Cont,V))  .
rinst(A, I:P:O, [], return(V))           ---say(I:P:O,ret(V))--->        rinst(A, nil, [], return(void)).
opres(I:P:O, [V|Vs])                     ---hear(I:P:O,res(V))--->       opres(I:P:O, Vs)               .
opres(I:P:O, [])                         ---hear(I:P:O,res(no_data))---> opres(I:P:O, [])
    :-    async_result(I:P:O)   .
opres(I:P:O, Vs)                         ---hear(I:P:O,ret(V))--->       opres(I:P:O, Vs1)
    :-    append(Vs,[V],Vs1)    .

%%%%% Spawning and terminating

rinst(A, nil, [], return(_V))            ---say(A,term)--->     []    .
runnable(A, T, Act, N)                   ---hear(A,term)--->    runnable(A, T, Act, N1)  :- N1 is N-1  .
% Note the "time left" state has to be zero for these rules to fire. 
runnable(A, 0, pending, N)               ---say(A,new)--->    [ runnable(A, T, idle, N1)
                                                              , rinst(A, nil, [], ap(Cont,void)) ]
    :-
    (N == 0 ; can_be_invoked_concurrently(A)),
    minimum_start_interval(A, T),
    implementation(A, Cont),
    N1 is N+1
    .
runnable(A, 0, serving([C|Cs],[V|Vs]), N) ---say(A,new)--->   [ runnable(A, T, serving(Cs,Vs), N1)
                                                              , rinst(A, C, [], ap(Cont,V)) ]
    :-
    (N == 0 ; can_be_invoked_concurrently(A)),
    minimum_start_interval(A, T), 
    implementation(A, Cont),
    N1 is N+1
    .

%%%%% Passing time

timer(A, 0)             ---say(A,tick)--->  timer(A, T)                :-    events(A, timing(T))    .
runnable(A, T, _, N)    ---hear(A,tick)---> runnable(A, T, pending, N) .

% The "time left" state V is always decreasing in delta(T) steps, and never negative.
runnable(A, V, Act, N)  ---delta(T)--->     runnable(A, V1, Act, N)    :-    V >= T, V1 is V-T    .
timer(A, V)             ---delta(T)--->     timer(A, V1)               :-    V >= T, V1 is V-T    .
rinst(A, C, XS, Code)   ---delta(_T)--->    rinst(A, C, XS, Code)      .
excl(A, V)              ---delta(_T)--->    excl(A, V)                 .
irv(A, V)               ---delta(_T)--->    irv(A, V)                  .
qelem(A, N, Vs)         ---delta(_T)--->    qelem(A, N, Vs)            .
delem(A, U, V)          ---delta(_T)--->    delem(A, U, V)             .
opres(A, Vs)            ---delta(_T)--->    opres(A, Vs)               .


%%%%% Ignoring broadcasts

ignore(_AL, rinst(_B,_C,_Xs,_Cont))   .
ignore(_AL, timer(_B,_T))             .

ignore((A,_L), runnable(B, _T, _Act, _N)) :- A \== B.
ignore((A,_L), excl(B,_V))                :- A \== B.
ignore((A,_L), irv(B,_V))                 :- A \== B.
ignore((A,_L), qelem(B,_N,_Vs))           :- A \== B, \+ connected(A,B).
ignore((A,_L), delem(B,_U,_V))            :- A \== B, \+ connected(A,B).
ignore((A,_L), opres(B,_Vs))              :- A \== B, \+ connected(A,B).


%%%%%  Helper predicate: flattening and evaluating reduction results

flateval([],              Q, Q)  :- !  .
flateval([ M | P ],       Q, R)  :- !, flateval(P, Q, R1),  flateval(M, R1, R)  .
flateval(rinst(A,C,Xs,M), Q, [ rinst(A,C,Xs,N) | Q ])  :- !, eval(M, N)   .
flateval(M,               Q, [ M               | Q ]).

% Experiment with "treelike" process soup
treeeval([],              [])         :- !  .
treeeval([ M | P ],       [M2 | P2])  :- !, treeeval(M, M2), treeeval(P, P2).
treeeval(rinst(A,C,Xs,M), [ rinst(A,C,Xs,N) | Q ])  :- !, eval(M, N)   .
treeeval(M,               [ M               | Q ]).

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

funWithArgsToEvaluate(H):- name(H,N), (append("rte_",_,N) ; N = "return").
