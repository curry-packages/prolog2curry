% to read this file with standard Prolog, uncomment the next two lines:
:- op(1150,fx,function).
function(_).

:- function insert/3.
insert(X,[],[X]).
insert(X,[Y|Ys],[X,Y|Ys]).
insert(X,[Y|Ys],[Y|Zs]) :- insert(X,Ys,Zs).

perm([],[]).
perm([X|Xs],Zs) :- perm(Xs,Ys), insert(X,Ys,Zs).

% less-or-equal relation on Peano numbers
:- function leq/2: []. % in order to keep it as a relation
leq(o,_).
leq(s(X),s(Y)) :- leq(X,Y).

sorted([]).
sorted([_]).
sorted([X,Y|Ys]) :- leq(X,Y), sorted([Y|Ys]).

psort(Xs,Ys) :- perm(Xs,Ys), sorted(Ys).

%%%%%%%%%%%%
descList(o,[]).
descList(s(N),[s(N)|Ns]) :- descList(N,Ns).

permsort_10(S) :-
        descList(s(s(s(s(s(s(s(s(s(s(o)))))))))),Xs),
        psort(Xs,S).

permsort_11(S) :-
        descList(s(s(s(s(s(s(s(s(s(s(s(o))))))))))),Xs),
        psort(Xs,S).

permsort_12(S) :-
        descList(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))),Xs),
        psort(Xs,S).

main(S) :- permsort_12(S).
