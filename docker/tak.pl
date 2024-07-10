% to read this file with standard Prolog, uncomment the next two lines:
:- op(1150,fx,function).
function(_).

:- function dec/2.
dec(s(X),X).

leq(o,_,true).
leq(s(_),o,false).
leq(s(X),s(Y),R) :- leq(X,Y,R).

:- function tak/4.
tak(X,Y,Z,A) :-	leq(X,Y,XLEQY), takc(XLEQY,X,Y,Z,A).

takc(true,X,Y,Z,Z).
takc(false,X,Y,Z,A) :-
        dec(X,X1),
        tak(X1,Y,Z,A1),
        dec(Y,Y1),
        tak(Y1,Z,X,A2),
        dec(Z,Z1),
        tak(Z1,X,Y,A3),
        tak(A1,A2,A3,A).
