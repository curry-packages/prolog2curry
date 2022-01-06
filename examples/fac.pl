% Factorial function as a predicate in Prolog.

% Since facp/2 is defined by overlapping rules, it will not be classified
% as a function. Therefore, we add the explicit function annotation.
:- function facp/2.
facp(0,1).
facp(N,F) :-
        N > 0,
        N1 is N - 1,
        facp(N1, F1),
        F is F1 * N.

% This version will be classified as a function and translated into
% an if-then-else expression.
fac(N,F) :-
        (N=0 -> F=1
              ; N1 is N - 1, fac(N1, F1), F is F1 * N).
