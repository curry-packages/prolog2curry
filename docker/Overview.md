This repository contains a translator which transforms
pure Prolog programs into [Curry](http://curry-lang.org) programs.


# How to use the docker image:

In order to translate a pure Prolog program to Curry,
the docker image is simply invoked with passing the Prolog program
on stdin, e.g.,

    > cat PROLOGFILE | docker run -i --rm currylang/prolog2curry

The informational output can be suppressed with option `-q`:

    > cat PROLOGFILE | docker run -i --rm currylang/prolog2curry -q

More details from the translation tools can be shown with option `-v3`:

    > cat PROLOGFILE | docker run -i --rm currylang/prolog2curry -v3

The transformation is performed in several steps where the failure
behavior of the Prolog programs is analyzed and taken into account
to ensure a semantically correct program transformation.
Details about the transformation implemented by this docker image
can be found in:

M. Hanus: [Improving Logic Programs by Adding Functions](http://dx.doi.org/10.1007/978-3-031-71294-4_2),
Proc. of the  34th International Symposium on Logic-based Program Synthesis
and Transformation (LOPSTR 2024), Springer LNCS 14919, pp. 27-44, 2024
