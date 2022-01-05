% Task: define a predicate sort(UL,SL) which is satisfied iff
% UL is a list and SL is a sorted list with the same elements of UL

% correct solution: SL is a sorted list: sorted(SL)
sorted([]).
sorted([_]).
sorted([E1,E2|L]) :- E1 =< E2, sorted([E2|L]).

% potential solutions: SL has the same elements as UL
% but in a possible different order
% SL is a permutation of UL: perm(UL,SL)
perm([],[]).
perm(L1,[E|R2]) :- remove(E,L1,R1), perm(R1,R2).

% remove(E,L,R): delete E in L and R is L without E
remove(E,[E|R],R).
remove(E,[F|R],[F|RwithoutE]) :- remove(E,R,RwithoutE).

% complete solution:
:- function sort/2.
sort(UL,SL) :-
        perm(UL,SL), % potential solution
        sorted(SL).  % correct solution
