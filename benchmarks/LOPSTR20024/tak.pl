% The next two lines are necessary to read this file with standard Prolog:
:- op(1150,fx,function).
function(_).

% for SWI-Prolog to avoid stack overflow:
?- current_prolog_flag(dialect,swi)
   -> set_prolog_flag(stack_limit, 4000000000)
    ; true.

add(o,N,N).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

double(X,R) :- add(X,X,R).

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

two(s(s(o))).
four(F) :- two(T), double(T,F).
n8(N) :- four(F), double(F,N).
n16(N) :- n8(F), double(F,N).
n24(N) :- n16(N16), n8(N8), add(N8,N16,N).
n27(N) :- n24(N24), two(N2), add(s(N2),N24,N).

goal0(R) :- n24(N24), n16(N16), n8(N8), tak(N24,N16,N8,R).
goal1(R) :- n27(N27), n16(N16), n8(N8), tak(N27,N16,N8,R).

main(R) :- goal1(R).
