:- use_module(library(lists)).

:- op(901, xfy, ':').
:- op(902, xfx, '!').
:- op(903, xfx, '?').
:- op(904, xfx, '--->').
:- op(905, xfx, '---').

combine( I:A?L, I:A!L, I:A!L ).
combine( I:A!L, I:A?L, I:A!L ).
combine( I:A?L, I:A?L, I:A?L ).
combine( delta(T), delta(T), delta(T)).


[]  ---L--->  []
    :-
    var(L)
    .
[P1|P2]  ---L--->  R
    :-    
    P1 ---L1---> Q1
    ,
    P2 ---L2---> Q2
    ,
    combine(L1,L2,L)
    ,
    flatten([Q1|Q2],R)
    .

%%%%% Exclusive areas

rinst(I:R, C, Xs, rte_enter(X,Cont))   ---I:X!enter--->     rinst(I:R, C, [X|Xs], cont(ok))
    .
excl(I:X, free)                        ---I:X?enter--->     excl(I:X, taken)
    .
rinst(I:R, C, X.Xs, rte_exit(X,Cont))  ---I:X!exit--->      rinst(I:R, C, Xs, cont(ok))
    .
excl(I:X, taken)                       ---I:X?exit--->      excl(I:X, free)
    .

%%%%% Inter-runnable variables

rinst(I:R, C, Xs, rte_irv_read(S,Cont))     ---I:S!irvr(V)--->    rinst(I:R, C, Xs, cont(V))
    .
irv(I:S, V)                                 ---I:S?irvr(V)--->    irv(I:S, V)
    .
rinst(I:R, C, Xs, rte_irv_write(S, Cont))   ---I:S!irvw(V)--->    rinst(I:R, C, Xs, cont(ok))
    .
irv(I:S, _)                                 ---I:S?irvw(V)--->    irv(I:S, V)
    .

%%%%% Sending/receiving

rinst(I:R, C, Xs, rte_receive(P,E,Cont))    ---I:P:E!rcv(V)--->           rinst(I:R, C, Xs, cont(v))
    .
qelem(I:P:E, N, V.Vs)                       ---I:P:E?rcv(V)--->           qelem(I:P:E, N, Vs)
    .
qelem(I:P:E, N, [])                         ---I:P:E?rcv(no_data)--->     qelem(I:P:E, N, [])
    % :-
    % async_rcv(P:E) in ...
    .
rinst(I:R, C, Xs, rte_send(P,E,V,Cont))     ---I:P:E!snd(V,Res)--->       rinst(I:R, C, Xs, cont(Res))
    .
qelem(I:P:E, N, Vs)                         ---I:P:E?snd(V,ok)--->        qelem(I:P:E, N, Vs1)
    :-
    length(Vs,X), X < N, append(Vs,[V],Vs1)
    .
qelem(I:P:E, N, Vs)                         ---I:P:E?snd(V,limit)--->     qelem(I:P:E, N, Vs)
    :-
    length(Vs,N)
    .
qelem(I:P:E, N, Vs)                         ---I:P:E?snd(V,Res)--->       qelem(I:P:E, N, Vs1)
    :-
    length(VS,X), X < N, append(Vs,[V],Vs1), Res \= ok
    .
runnable(I:R, T, _, N)                      ---I:P:E?snd(V,ok)--->        runnable(I:R, T, pending, N)
    % :-
    % data_received(P:E) in events(I:R)
    .
runnable(I:R, T, Act, N)                    ---I:P:E?snd(V,limit)--->     runnable(I:R, T, Act, N)
    % :-
    % data_received(P:E) in events(I:R)
    .

%%%%% Reading/writing

rinst(I:R, C, XS, rte_read(P,E,Cont))           ---I:P:E!rd(V)--->    rinst(I:R, C, XS, cont(V))
    .
delem(I:P:E, U, V)                              ---I:P:E?rd(V)--->    delem(I:P:E, false, V)
    .
rinst(I:R, C, XS, rte_write(P,E,V,Cont))        ---I:P:E!wr(V)--->    rinst(I:R, C, XS, cont(ok))
    .
delem(I:P:E, V, _)                              ---I:P:E?wr(V)--->    delem(I:P:E, true, V)
    .
runnable(I:R, T, _, N)                          ---I:P:E?wr(V)--->    runnable(I:R, T, pending, N)
    % :-
    % data_received(P:E) in events(I:R)
    .
rinst(I:R, C, XS, rte_is_updated(P,E,Cont))     ---I:P:E!up(U)--->    rinst(I:R, C, XS, cont(U))
    .
delem(I:P:E, U, V)                              ---I:P:E?up(U)--->    delem(I:P:E, U, V)
    .
rinst(I:R, C, XS, rte_invalidate(P,E,Cont))     ---I:P:E!inv--->      rinst(I:R, C, XS, cont(ok))
    .
delem(I:P:E, U, _)                              ---I:P:E?inv--->      delem(I:P:E, true, invalid)
    .

%%%%% Calling a server

rinst(I:R, C, XS, rte_call(P,O,V,Cont))     ---I:P:O!call(V,I:P:O,Res)--->    rinst(I:R, C, XS, cont(Res))
    :-
    % async(P:O) in server_call_points(I:R)
    % ,
    Res \= ok
    .
rinst(I:R, C, XS, rte_call(P,O,V,Cont))     ---I:P:O!call(V,I:P:O,ok)--->     rinst(I:R, C, XS, rte_result(P,O,Cont))
    % :-
    % sync(P:O) in server_call_points(I:R)
    .
runnable(I:R, T, serving(Cs,Vs), N)         ---I:P:O?call(V,C,ok)--->         runnable(I:R, T, serving(Cs1,Vs1), N)
    :-
    % op_invoked(P:O) in events(I:R)
    % ,
    not member(C,Cs), append(Cs,[C],Cs1), append(Vs,[V],Vs1)
    .
runnable(I:R, T, serving(Cs,Vs), N)         ---I:P:O?call(V,C,limit)--->      runnable(I:R, T, serving(Cs,Vs), N)
    :-
    member(C, Cs)
    .

%%%%% Obtaining a server result

rinst(I:R, C, XS, rte_result(P,O,Cont))     ---I:P:O!res(V)--->           rinst(I:R, C, XS, cont(V))
    .
opres(I:P:O, V.Vs)                          ---I:P:O?res(V)--->           opres(I:P:O, Vs)
    .
opres(I:P:O, [])                            ---I:P:O?res(no_data)--->     opres(I:P:O, [])
    % :-
    % async_res(P:O) in ...
    .
rinst(A, I:P:O, [], rte_terminate(V))       ---I:P:O!ret(V)--->           rinst(A, -, [], rte_terminate(void))
    .
opres(I:P:O, Vs)                            ---I:P:O?ret(V)--->           opres(I:P:O, Vs1)
    :-
    append(Vs,[V],Vs1)
    .

%%%%% Spawning and terminating

rinst(A, -, [], rte_terminate(V))        ---A!term--->  []
    .
runnable(A, T, Act, N)                   ---A?term--->  runnable(A, T, Act, N1)
    :-
    N1 is N-1
    .
runnable(A, 0, pending, N)               ---A!new--->   [ runnable(A, T, idle, N1), rinst(A, -, [], cont(void)) ]
    :-
    N == 0 % or can_be_invoked_concurrently(A)
    ,
    % T = minimum_start_interval(A), Cont = implementation(A)
    N1 is N+1
    .
runnable(A, 0, serving(C.Cs,V.Vs), N)    ---A!new--->   [ runnable(A, T, serving(Cs,Vs), N1), rinst(A, C, [], cont(V)) ]
    :-
    N == 0 % or can_be_invoked_concurrently(A)
    ,
    % T = minimum_start_interval(A), Cont = implementation(A)
    N1 is N+1
    .

%%%%% Passing time

timer(A, 0)             ---A!tick--->   timer(A, T)
    % :-
    % timing(T) in events(A)
    .
runnable(A, T, _, N)    ---A?tick--->   runnable(A, T, pending, N)
    .
runnable(A, T, Act, N)  ---delta(V)--->   runnable(A, T1, Act, N)
    :-
    T >= V, T1 is T-V
    .
timer(A, T)             ---delta(V)--->   timer(A, T1)
    :-
    T >= V, T1 is T-V
    .
rinst(A, XS, Code)      ---delta(V)--->   rinst(A, XS, Code)
    .
excl(A, V)              ---delta(V)--->   excl(A, V)
    .
irv(A, V)               ---delta(V)--->   irv(A, V)
    .
qelem(A, N, Vs)         ---delta(V)--->   qelem(A, N, Vs)
    .
delem(A, U, V)          ---delta(V)--->   delem(A, U, V)
    .
opres(A, Vs)            ---delta(V)--->   opres(A, Vs)
    .
