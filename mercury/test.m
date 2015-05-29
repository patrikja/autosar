:- module test.
:- interface.
:- pred eq(T,T).
:- mode eq(in,out) is det.
:- mode eq(out,in) is det.
:- type person ---> patrik; tunde; julia; endre.
:- pred parent(person,person).
:- pred sibling(person,person).
:- pred cousin(person,person).
:- implementation.
eq(X,X).
parent(patrik,julia).
parent(patrik,endre).
parent(tunde,julia).
parent(tunde,endre).
sibling(X,Y) :- parent(P,X), parent(P,Y).
cousin(X,Y) :- parent(P,X), parent(Q,Y), sibling(P,Q).
%
% % Test file to check the parser on simple inputs
%
% X ==> Y.
% X:Y --- L ---> P.
% a:b:c ==> d:e:f.
% a:a0 ==> X.
% _ ==> _.
% e:p0:i ==> e:p3:i.
% _ ==> _ :- false.
% event(r1:i, timing(300)).
% event(r1:i, data_received(e:p3)).
% X = [A|List].
% example1(L,R) :- A ---L ---> R.
% X :- A ; B.
