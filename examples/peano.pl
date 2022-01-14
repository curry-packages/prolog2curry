% Some arithmetic operations on natural numbers in Peano represention
% (compare Sterlin/Shapiro: The Art of Prolog)

natural_number(o).
natural_number(s(N)) :- natural_number(N).

% plus(X,Y,Z) <=> Z is the sum of X and Y
plus(o,Y,Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

% times(X,Y,Z) <=> Z is the product of X and Y
times(o,_,o).
times(s(X),Y,Z) :- times(X,Y,XY), plus(XY,Y,Z).

% less-or-equal relation
:- function leq/2: []. % in order to keep it as a relation
leq(o,_).
leq(s(X),s(Y)) :- leq(X,Y).

% exp(N,X,Y) <=> Y equals X raised to the power N
exp(o,_,s(o)). % thus, exp(o,o,s(o)) holds rather being undefined
exp(s(N),X,Y) :- exp(N,X,Z), times(Z,X,Y).

% factorial(N,F) <=> F equals the factorial of N
factorial(o,s(o)).
factorial(s(N),F) :- factorial(N,F1), times(s(N),F1,F).

% for testing:
three(s(s(s(o)))).

fac3(F3) :- three(N3), factorial(N3,F3).
