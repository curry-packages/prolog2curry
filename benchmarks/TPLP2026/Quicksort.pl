% generated: 16 November 1989
% to read this file with standard Prolog, uncomment the next two lines:
:- op(1150,fx,function).
function(_).

% Implementation of quicksort based on partitioning a list
% into smaller and larger elements.

qsort([],[]).
qsort([X|L],S) :-
	partition(X,L,L1,L2),
	qsort(L1,S1),
	qsort(L2,S2),
	app(S1,[X|S2],S).

:- function partition/4: [3,4].
partition(_,[],[],[]).
partition(Y,[X|L],Xs,Ys) :-
	X =< Y -> (partition(Y,L,L1,L2), Xs=[X|L1], Ys=L2)
                ; (partition(Y,L,L1,L2), Xs=L1, Ys=[X|L2]).

app([],Xs,Xs).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).


% quicksort a list of 6400 integers

main(S) :-
        L= [27,74,17,33,94,18,46,83,65,2,
	    32,53,28,85,99,47,28,82, 6,11,
	    55,29,39,81,90,37,10, 0,66,51,
	     7,21,85,27,31,63,75, 4,95,99,
	    11,28,61,74,18,92,40,53,59, 8],
        app(L,L,L1), app(L1,L1,L2), app(L2,L2,L3), app(L3,L3,L4),
        app(L4,L4,L5), app(L5,L5,L6), app(L6,L6,L7),
        qsort(L7,S).
