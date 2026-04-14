% to read this file with standard Prolog, uncomment the next two lines:
:- op(1150,fx,function).
function(_).

% Placing queens on a chess board via incremental selection and testing.
%
% idea taken from Sterling and Shapiro, "The Art of Prolog," page 211

:- function queens/2.
queens(N,Qs) :-
        range(s(o),N,Ns), selectqueens(Ns,[],Qs).

:- function selectqueens/3.
selectqueens([],Qs,Qs).
selectqueens(UnplacedQs,SafeQs,Qs) :-
	select(Q,UnplacedQs,UnplacedQs1),
	noattack(s(o),Q,SafeQs),
	selectqueens(UnplacedQs1,[Q|SafeQs],Qs).

noattack(_,_,[]).
noattack(N,X,[Y|Ys]) :-
        neq(X,Y), plus(Y,N,YpN), neq(X,YpN), plus(X,N,XpN), neq(XpN,Y),
        noattack(s(N),X,Ys).

% select some element from a list
:- function select/3.
select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

% Relate M and N with a list of Peano numbers between M and N inclusive
:- function range/3.
range(M,N,R) :- leq(M,N,B), range_leq(B,M,N,R).
range_leq(true,M,N,[M|Ns]) :- range(s(M),N,Ns).
range_leq(false,_,_,[]).

% addition on Peano numbers
plus(o,N,N).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

% not equal on Peano numbers
neq(o,s(_)).
neq(s(_),o).
neq(s(X),s(Y)) :- neq(X,Y).

% less-than-or-equal on Peano numbers
leq(o,_,true).
leq(s(_),o,false).
leq(s(X),s(Y),R) :- leq(X,Y,R).

queens4(Qs) :- queens(s(s(s(s(o)))),Qs).
queens8(Qs) :- queens(s(s(s(s(s(s(s(s(o)))))))),Qs).

main(Qs) :- queens8(Qs).
