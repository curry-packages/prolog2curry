% to read this file with standard Prolog, uncomment the next two lines:
:- op(1150,fx,function).
function(_).

plus(o,Y,Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

plus3(X,Y,Z,R) :- plus(X,Y,XY), plus(XY,Z,R).

:- function thirdOf/2.
thirdOf(X,Y) :- plus3(Y,Y,Y,X).

main(T) :- thirdOf(o,T).
