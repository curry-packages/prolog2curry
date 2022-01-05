% Natural numbers in Peano represention: o (zero), s (successor)
% s(s(s(o))) represents "three"

isPeano(o).
isPeano(s(N)) :- isPeano(N).

% add(X,Y,Z) <=> Z is the sum of X and Y
add(o,Y,Y).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

% sub(X,Y,Z) <=> Z is the difference of X and Y (<=> add(Y,Z,X))
:- function sub/3.
sub(X,Y,Z) :- add(Y,Z,X).

% mult(X,Y,Z) <=> Z is the product of X and Y
mult(o,_,o).
mult(s(X),Y,Z) :- mult(X,Y,XY), add(XY,Y,Z).

% less-or-equal relation
:- function leq/2: []. % in order to keep it as a relation
leq(o,_).
leq(s(X),s(Y)) :- leq(X,Y).

% for testing:
three(s(s(s(o)))).

% add three numbers:
add3(X,Y,Z,R) :- add(X,Y,S), add(S,Z,R).

% This query has an infinite search space in Prolog:
% ?- add3(X,Y,Z,o).

% The equivalent query in the transformed Curry program is finite:
% > add3 x y z =:= O  where x,y,z free
