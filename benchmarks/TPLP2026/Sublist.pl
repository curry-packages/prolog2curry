% to read this file with standard Prolog:
:- op(1150,fx,function).
function(_).

% Concatenate two lists:
app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

% Concatenate three lists:
app3(Xs,Ys,Zs,Ts) :- app(Xs,Ys,Rs), app(Rs,Zs,Ts).

% sublist/2 is satisfied if the second argument is a sublist of the first.
:- function sublist/2.
sublist(Xs,Sub) :- app3(_,Sub,_,Xs).

% This query does not terminate after showing six answers
% whereas the Curry-translated version has a finite search space.
main(S) :- sublist([1,2],S).
