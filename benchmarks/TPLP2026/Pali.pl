% to read this file with standard Prolog:
:- op(1150,fx,function).
function(_).

% Concatenate two lists:
app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

% Concatenate three lists:
app3(Xs,Ys,Zs,Ts) :- app(Xs,Ys,Rs), app(Rs,Zs,Ts).

% Relate a list to its reversed version:
rev([],[]).
rev([X|Xs],Zs) :- rev(Xs,Ys), app(Ys,[X],Zs).

% pali/2 is satisfied if the first argument is a palindrome with the
% second argument as a middle element.
:- function pali/2.
pali(Zs,X) :- app3(Xs,[X],Ys,Zs), rev(Xs,Ys).

% This query does not terminate after showing one answer
% whereas the Curry-translated version has a finite search space.
main(M) :- pali([1,2,3,2,1],M).
