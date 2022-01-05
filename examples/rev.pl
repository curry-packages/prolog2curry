% to read this file with standard Prolog, uncomment the next two lines:
%:- op(1150,fx,function).
%function(_).

% app(L1,L2,L3) <=> list concatenation
% :- function app/3: 3.
app([],L,L).
app([E|R],L,[E|RL]) :- app(R,L,RL).

% reverse a list.
% :- function rev/2: [2].
rev([],[]).
rev([E|R],RevList) :- rev(R,RL), app(RL,[E],RevList).

%rev9(L) :- rev([1,2,3,4,5,6,7,8,9],L). % if you use Curry lists
rev3(L) :- rev([a,b,c],L).   % if you do not use Curry lists (--nolists)

