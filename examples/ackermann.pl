% Sterlin/Shapiro: The Art of Prolog, program 3.9

% in order to infer the function automatically, show that ackermann is
% inductively sequential.
:- function ackermann/3.
ackermann(o,N,s(N)).
ackermann(s(M),o,Val) :- ackermann(M,s(o),Val).
ackermann(s(M),s(N),Val) :- ackermann(s(M),N,Val1), ackermann(M,Val1,Val).

