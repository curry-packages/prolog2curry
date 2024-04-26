% Type definitions for natural numbers and polymorphic binary trees:

:- type nat = s(nat) ; o.
:- type tree(A) = leaf(A) ; node(tree(A),tree(A)).

num_leaves(leaf(_),s(o)).
num_leaves(node(M1,M2),s(N)) :-
        num_leaves(M1,N1), num_leaves(M2,N2), add(N1,N2,N).

add(o,N,N).
add(s(M),N,s(Z)) :- add(M,N,Z).

% This query terminates:
goal1(N) :-num_leaves(node(leaf(s(s(o))),node(leaf(s(o)),leaf(s(o)))),N).

% This query does not terminate after showing one answer:
goal2(T) :- num_leaves(T,s(s(s(o)))).

% NOTE: the Curry-translated version of goal2 has a finite search space!
