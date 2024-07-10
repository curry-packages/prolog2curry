prolog2curry - Transforming Prolog programs to Curry programs
=============================================================

This package contains an implementation of a tool (`pl2curry`)
to transform Prolog programs to Curry programs.
The idea of this tool is to demonstrate the advantages of
functional logic languages compared to purely logic languages.
Thus, the tool translates only pure logic programs (without side
effecting predicates etc).
The initial ideas of this tool are described in detail in a
[paper presented at ICLP 2022 and published in TPLP](http://doi.org/10.1017/S1471068422000187).

Tool options
------------

The tool has various options to influence the kind of transformation, e.g.:

- `--conservative`: transform each Prolog predicate into a Curry predicate

  Note that this is always possible since non-linear left-hand sides
  are allowed in Curry (in contrast to Haskell).

- with functions (this is the default provided that `--conservative` 
  is not set): specify some predicates as functions by:

  * `:- function p/n.`: The last argument is the result.
  * `:- function p/n: i.`: The i-th argument is the result (i=n: last argument).
  * `:- function p/n: [i1,...,ik].`: Argument positions [i1,...,ik] are
    put as result arguments.

  For instance, if `p/3` is a function and the last argument is the result,
  then a goal `p(X,Y,Z)` is transformed into `z =:= p x y`.

  Note that it is necessary to define `function` as an operator in Prolog
  in order to read Prolog programs with such directives. This can be done
  by adding the following directives at the beginning of the Prolog program:

      :- op(1150,fx,function).
      function(_).

- with demand (this is the default provided that `--nodemand`
  is not set):
  similarly to functions, but function calls are transformed
  into local variable bindings rather than unifications. Hence,
  functions are evaluated only if its result is demanded
  (due to Curry's lazy evaluation strategy).

- with inlining (this is the default provided that `--noinline`
  is not set):
  similarly to demand, but bindings are inlined (if possible)
  to obtain a more compact source code

Since adding `function` directives to specify result argument positions
is tedious, the tool also contains an analysis to derive automatic
`function` directives (if not already explicitly provided and
if the option `--noanalysis` is not set) for standard functions
(i.e., where only the last argument is a result position).
It is based on the following principle:

- If `p` is an n-ary predicate and there is a (minimal) set of
  argument position such that the rules for `p` are inductively
  sequentially defined on this set of argument positions
  (in particular, non-overlapping w.r.t. these arguments),
  `p` is considered as a function (where the last argument
  is the result argument position, or the maximum of the remaining
  argument positions if the option `--anyresult` is set).

The information about the inferred sets of inductively sequential argument
and result arguments of functions is printed if the verbosity is larger than 2.

A special case of the previous criterion are predicates
defined by a single rule, e.g., predicates which define constants, as

    two(s(s(o))).

This will be translated into

    two = S (S O)

Although this is correct, it is sometimes unintended, e.g.,
if `p` is defined by the single clause

    p(X,Y) :- q(X,Z), r(Z,Y).

In order to keep such predicates as predicates on the Curry level,
the following heuristic is used. A predicate defined by a single rule
is transformed into a function only if the last argument in the
left-hand side is not a variable or a variable which occurs in a
result argument position in the rule's body.

Although these heuristics provide expected transformations
in most case, one can always override them using an explicit
`function` directive.


Type declarations:
------------------

To provide a more reasonable translation to Curry,
the translation tool considers also type declarations.
These declarations are similarly to polymoprhic algebraic data types
but use a Prolog-like syntax.
For instance, the Prolog program could contain the directives

    :- type nat = o ; s(nat).
    :- type tree(A) = leaf(A) ; node(tree(A),tree(A)).

These are translated into the Curry type declarations

    data Nat = O | S Nat
     deriving (Eq,Show)

    data Tree a = Leaf a | Node (Tree a) (Tree a)
     deriving (Eq,Show)

All other constructors occurring in the logic program
(except for `true` and `false` and list constructors, see below)
are declared in a single type named `Term` in Curry.

Note that it is necessary to define `type` as an operator in Prolog
in order to read Prolog programs with such directives. This can be done
by adding the following directives at the beginning of the Prolog program:

    :- op(1150,fx,type).
    type(_).

Fail-sensitive transformation:
------------------------------

Due to the fact that Prolog programs are transformed (in the default case)
into nested functions which are lazily evaluated in Curry,
it might be the case that the Curry program computes more answers
than the original Prolog program.
This might be the case if a failing or non-terminating
predicate is evaluated in Prolog but not demanded in the transformed
Curry program. In general, this can be considered as an advantage
of functional logic programming compared to pure logic programming.
However, if it is intended to keep the same answer semantics
between Prolog and the generated Curry programs,
one can specify a set of _failing functions_ (i.e., functions that
are not totally defined due to partial pattern matching or
infinite computations).
In this case, any occurrence of such a failing function will be strictly
evaluated in the Curry program. This _fail-sensitvie transformation_
will be used if the option `--failfuncs=F` is provided.
In this case, the file `F` contains in each line a failing function
in the form `Mod f`, i.e., `f` is a function defined in module `Mod`.
Such files can be generated by the tool `curry-calltypes` by adding
the option `--storefunc` (see Curry package `verify-non-fail`).
The shell script `scripts/pl2curry-failsensitive.sh`
can be used to apply the fail-sensitive transformation with these tools.

More details about this fail-sensitive transformation can be found in:

M. Hanus: Improving Logic Programs by Adding Functions,
Proc. of the  34th International Symposium on Logic-based Program Synthesis
and Transformation (LOPSTR 2024),
to appear in Springer LNCS, 2024

Examples for this transformation and benchmarks can be found
in the directory `benchmarks/LOPSTR2024`.


Technical remarks:
------------------

- Prolog atoms `true` and `false` are translated into the Curry
  Boolean constants `True` and `False`.

- Prolog lists are transformed into Curry lists (unless the option
  `--nolists` is set).
  This should yield the intended code but might produce type errors
  for strange uses of Prolog lists, e.g., `.(1,.(2,3))`.
  If Curry lists are not used (by setting the option `--nolists`),
  Prolog lists are transformed into Curry terms by using the constructors
  `NIL` and `CONS`, e.g., the Prolog list
  `[1,2]` is transformed into `CONS 1 (CONS 2 NIL)`.

- In the default case, only the last argument of a predicate will be
  considered as a result argument position for inferred functions.
  Hence, if the last argument is contained in a minimal set
  of inductively sequential arguments, it will not be transformed
  into a function.

  This behavior can be changed by setting the option `--anyresult`.
  In this case, a maximum argument will be selected.
  If this is not the last one, the index of the result argument
  position is added to indicate that the order of arguments
  has been changed in the transformed function.
  See `examples/demand.pl` for such examples.
