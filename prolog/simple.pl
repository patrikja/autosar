% minimal test example
step(par(P1,Q1),say(A), par(P2,Q2)) :- step(P1,say(A), P2), step(Q1,hear(A),Q2).
step(par(P1,Q1),say(A), par(P2,Q2)) :- step(P1,hear(A),P2), step(Q1,say(A), Q2).
step(par(P1,Q1),hear(A),par(P2,Q2)) :- step(P1,hear(A),P2), step(Q1,hear(A),Q2).

step(talker(I),  say(I), talker(succ(I))).
step(talker(I),  hear(J),talker(I)).
step(listener(X),hear(I),listener(I)).
step(listener(X),hear(I),listener(X)).

% trace of the transitive, reflexive closure
trace(X,cons(L,Ls),Z) :- step(X,L,Y),trace(Y,Ls,Z).
trace(X,nil,X).

%Questions to answer:

% Can we reach a certain state:
$answer(Ls) :- trace(par(talker(zero),listener(zero)),
                     Ls,
                     par(talker(succ(succ(succ(zero)))),listener(zero))).

% This should be impossible (only one may talk in one step). But eprover takes a long time...
%~trace(par(talker(zero),talker(zero)),
%                   cons(say(zero),nil),
%                   par(talker(succ(zero)),talker(succ(zero)))).


%A simple extension
%step(delayedtalker(X,Y),hear(X),talker(Y)).
%step(delayedtalker(X,Y),hear(Z),delayedtalker(X,Y)) :- ~equal(X,Z).

%$answer(R) :- trace(par(talker(zero),delayedtalker(zero,zero)),
%                    cons(say(zero),cons(say(succ(zero)),cons(L,cons(say(zero),nil)))),
%                    R).
