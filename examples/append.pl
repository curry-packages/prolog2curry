% app(L1,L2,L3) <=> list concatenation
app([]   ,L,L     ).
app([E|R],L,[E|RL]) :- app(R,L,RL).

% append three lists:
dapp(As,Bs,Cs,Ds) :- app(As,Bs,Xs), app(Xs,Cs,Ds).

% This query has an infinite search space in Prolog:
% ?- dapp(Xs,Ys,Zs,[]).

% The equivalent query in the transformed Curry program is finite:
% > dapp xs ys zs =:= []  where xs,ys,zs free
