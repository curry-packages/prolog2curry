prolog2curry - Transforming Prolog programs to Curry programs
=============================================================

This package contains an implementation of a tool (`pl2curry`)
to transform Prolog programs to Curry programs. The tool has various
options to influence the kind of transformation, e.g.:

- `--conservative`: transform each Prolog predicate into a Curry predicate

  Note that this is always possible since non-linear left-hand sides
  are allowed in Curry (in contrast to Haskell).

- with functions (default, i.e., `--conservative` is not set):
  specify some predicates as functions by:

  * `:- function p/n.`: The last argument is the result.
  * `:- function p/n: i.`: The i-th argument is the result (i=n: last argument).
  * `:- function p/n: [i1,...,ik].`: Argument positions [i1,...,ik] are
    put as result arguments.

  For instance, if `p/3` is a function and the last argument is the result,
  then a goal `p(X,Y,Z)` is transformed into `z =:= p x y`.

- with demand (default, i.e., `--nodemand` is not set):
  similarly to functions, but function calls are transformed
  into local variable bindings rather than unifications. Hence,
  functions are evaluated only if its result is demanded
  (due to Curry's lazy evaluation strategy).

- with inlining (default, i.e., `--noinline` is not set):
  similarly to demand, but bindings are inlined to obtain
  a more compact source code

- use lists (default, i.e., `--nolists` is not set):
  Prolog lists are transformed into Curry lists.
  This should yield the intended code but might produce type errors
  for strange uses of Prolog lists, e.g., `.(1,.(2,3))`.
  If Curry lists are not used, Prolog lists are transformed in Curry
  by using the constructors `NIL` and `CONS`, e.g., the Prolog list
  `[1,2]` is transformed into `CONS 1 (CONS 2 NIL)`.

Since adding `function` directives to specify result argument positions
is tedious, the tool also contains an analysis to derive automatic
`function` directives (if not already explicitly provided and
if the option `--noanalysis` is not set) for standard functions
(i.e., where only the last argument is a result position).
It is based on the following principle:

- If `p` is an n-ary predicate and the rules for `p` are pairwise
  non-overlapping w.r.t. the first (n-1) arguments, `p` is considered
  as a function.

A special case of the previous criterion are predicates
defined by a single rule, e.g., defining constants by

    two(s(s(o))).

which is translated into

    two = S (S O)

Although this is correct, it is sometimes unintended, e.g.,
if `p` is defined by the single clause

    p(X,Y) :- q(X,Z), r(Z,Y).

In order to keep such predicates as predicates on the Curry level,
the following heuristic is used. A predicate defined by a single rule
is transformed as a function only if the last argument in the left-hand side
is not a variable or a variable which occurs in a result argument position
in the rule's body.

Although these heuristics provide expected transformations
in most case, one can always override them using an explicit
`function` directive.
