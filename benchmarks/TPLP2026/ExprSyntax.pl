% to read this file with standard Prolog, uncomment the next four lines:
:- op(1150,fx,function).
function(_).

% Predicates to specify the syntax of simple arithmetic expressions.
% The argument of each syntax predicate is a list of tokens
% (zero, one, plus, times, leftbr, rightbr)
% which is a valid sentence of this syntactic construct.
%
% This is the Prolog version of an example shown in
% Gonzalez-Moreno, Hortala-Gonzalez, Lopez-Fraguas, Rodriguez-Artalejo:
% An approach to declarative programming based on a rewriting logic
% Journal of Logic Programming 40, pp. 47-87, 1999
% DOI: 10.1016/S0743-1066(98)10029-8

:- function exp/1.
exp(S) :- term(S).
exp(S) :- term(S1),  exp(S2), app(S1,[plus|S2],S).

:- function term/1.
term(S) :- factor(S).
term(S) :- factor(S1),  term(S2), app(S1,[times|S2],S).

:- function factor/1.
factor([zero]).
factor([one]).
factor([leftbr|S]) :- exp(SE), app(SE,[rightbr],S).


% Concatenate two lists:
app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

% Parsing: check whether a given sentence is an expression
% (does not terminate in Prolog)
parse_exp :- exp([zero,plus,one]).

% Language generation: generate sentences with a fixed length
% (does not terminate after generating a few sentences)
gen_exp(S) :- S=[_,_,_,_,_], exp(S).


main(S) :- gen_exp(S).
