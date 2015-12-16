% minimal test example
step(par(P1,Q1),say(A), par(P2,Q2)) :- step(P1,say(A), P2),step(Q1,hear(A),Q2).
step(par(P1,Q1),say(A), par(P2,Q2)) :- step(P1,hear(A),P2),step(Q1,say(A), Q2).
step(par(P1,Q1),hear(A),par(P2,Q2)) :- step(P1,hear(A),P2),step(Q1,hear(A),Q2).

step(talker(I),  say(I), talker(succ(I))).
step(talker(I),  hear(J),talker(I)).
step(listener(X),hear(I),listener(I)).
step(listener(X),hear(I),listener(X)).

% trace of the transitive, reflexive closure
trace(X,cons(L,Ls),Z) :- step(X,L,Y),trace(Y,Ls,Z).
trace(X,nil,X).

%A simple extension
step(delayedtalker(X,Y),hear(X),talker(Y)).
step(delayedtalker(X,Y),hear(Z),delayedtalker(X,Y)) :- ~equal(X,Z).

%Questions to answer:

% Can we reach a certain state:
:- trace(par(talker(T),listener(L)),
         Ls,
         par(talker(succ(succ(succ(T)))),listener(L))).

% This should be impossible (only one may talk in one step)
:- trace(par(talker(T),talker(T)),
         cons(say(T),nil),
         par(talker(succ(T)),talker(succ(T)))).

:- trace(par(talker(zero),delayedtalker(zero,zero)),
         Ls,
         After), sublist(cons(say(succ(zero)),cons(say(zero),nil)),Ls).

% library code

sublist(nil,Xs).
sublist(cons(X,Xs),cons(Y,Ys)) :- sublist(cons(X,Xs),Ys).
sublist(cons(X,Xs),cons(X,Ys)) :- sublist(Xs,Ys).
