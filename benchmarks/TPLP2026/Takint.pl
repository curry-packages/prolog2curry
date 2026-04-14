% The following two lines are necessary for loading into standard Prolog:
:- op(1150,fx,function).
function(_).

%   tak benchmark (Takeuchi function testing recursive arithmetic)
%
%   by Evan Tick (from Lisp version by R. P. Gabriel)

:- function tak/4.
tak(X,Y,Z,A) :-
	X =< Y
        -> A=Z
	 ; ((X1 is X - 1),
            tak(X1,Y,Z,A1),
            (Y1 is Y - 1),
            tak(Y1,Z,X,A2),
            (Z1 is Z - 1),
            tak(Z1,X,Y,A3),
            tak(A1,A2,A3,A)).

takInt_24_16_8(R) :- tak(24,16,8,R).
takInt_27_16_8(R) :- tak(27,16,8,R).
takInt_33_17_8(R) :- tak(33,17,8,R).

main(R) :- takInt_27_16_8(R).
