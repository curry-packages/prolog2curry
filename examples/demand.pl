% Tests for demanded argument analysis

% This will be transformed into a function from the first to the second
% argument.
p(a,a).
p(b,_).

% This will be turned into a function having the second argument as input
% and returning the first argument only if option `--anyresult` is set.
q(a,a).
q(_,b).

% This will be transformed into a function although all single arguments
% positions are overlapping. However, the argument set {1,2} and {1,3}
% are minimal inductively sequential sets. Hence, it will be transformed
% into a function into the last argument.
r(o,_,o).
r(s(M),o,s(M)).
r(s(M),s(N),s(M)).

% Here, the argument sets {1,3} and {2,3} are minimal inductively sequential
% sets. Since the last argument always contains to these sets,
% it will transformed into a function (with result argument 2)
% only if option `--anyresult` is set.
s(o,o,_).
s(s(M),s(M),o).
s(s(M),s(M),s(N)).

