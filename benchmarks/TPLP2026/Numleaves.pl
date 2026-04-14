% to read this file with standard Prolog, uncomment the next four lines:
:- op(1150,fx,function).
function(_).
:- op(1150,fx,type).
type(_).

% Type definitions for natural numbers and polymorphic binary trees:

:- type nat = o ; s(nat).
:- type tree(A) = leaf(A) ; node(tree(A),tree(A)).

numleaves(leaf(_),s(o)).
numleaves(node(M1,M2),s(N)) :-
        numleaves(M1,N1), numleaves(M2,N2), plus(N1,N2,N).

plus(o,N,N).
plus(s(M),N,s(Z)) :- plus(M,N,Z).

% This query does not terminate after showing one answer
% whereas the Curry-translated version has a finite search space.
:- function numleaves_7/1.
numleaves_7(T) :- numleaves(T,s(s(s(s(s(s(s(o)))))))).

main(T) :- numleaves_7(T).
