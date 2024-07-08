% The next two lines are necessary to read this file with standard Prolog:
:- op(1150,fx,function).
function(_).

signat(o,o).
signat(s(X),s(o)).

plus(o,Y,Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

:- function main/1.
main(X) :- plus(X,o,N), signat(N,s(o)).
