% Sterlin/Shapiro: The Art of Prolog, program 3.9

% the definition is inductively sequential so that it will be classified
% as a function
ackermann(o,N,s(N)).
ackermann(s(M),o,Val) :- ackermann(M,s(o),Val).
ackermann(s(M),s(N),Val) :- ackermann(s(M),N,Val1), ackermann(M,Val1,Val).

