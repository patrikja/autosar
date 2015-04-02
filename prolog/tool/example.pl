
eq(X,X).
parent(patrik,julia).
parent(patrik,endre).
parent(tünde,julia).
parent(tünde,endre).
sibling(X,Y) :- parent(P,X), parent(P,Y).
cousing(X,Y) :- parent(P,X), parent(Q,Y), sibling(P,Q).

% Test file to check the parser on simple inputs

X ==> Y.
X:Y --- L ---> P.
a:b:c ==> d:e:f.
a:a0 ==> X.
_ ==> _.
e:p0:i ==> e:p3:i.
_ ==> _ :- false.
event(r1:i, timing(300)).
event(r1:i, data_received(e:p3)).
X = [A|List].
example1(L,R) :- A ---L ---> R.
X :- A ; B.
