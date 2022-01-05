% Tests for demanded argument analysis

% This will be transformed in a function from the first to the second
% argument.
p(a,a).
p(b,_).

% This will be turned into a function having the second argument as input
% and returning the first argument only if option `--anyresult` is set.
q(a,a).
q(_,b).

