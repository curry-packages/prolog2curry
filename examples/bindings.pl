% Examples to test correct transformation of bindings

% in this case, the different bindings in the result
% arguments of q and r must be translated into unifications.
:- function p/2.
p(a,X) :- q(X), r(X).

q(b).
r(a).

% Example for multiple result arguments
:- function s/3: [2,3].
s(a,b,c).

:- function t/1: [].
t(Z) :- s(X,Y,Z).
