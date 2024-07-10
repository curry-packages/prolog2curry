Docker image of the Prolog2Curry translator
===========================================

This directory contains some files to create and run the
[Docker image of prolog2curry](https://hub.docker.com/r/currylang/prolog2curry).


Building a new docker image
---------------------------

If necessary, clean the old image:

    > docker image rm prolog2curry
    > docker image prune

Then build the docker image:

    > docker build -t prolog2curry .


Uploading image to Docker Hub
-----------------------------

When the repository does not yet exist on Docker Hub:

1. Log in on https://hub.docker.com as "currylang"
2. Click on "Create Repository"
3. Choose a name ("prolog2curry") and click create

When the repository exists on Docker Hub:

Log into the Docker Hub from command line, tag and push the local image:

    > docker login --username currylang
    > docker tag prolog2curry currylang/prolog2curry:<version>
    > docker push currylang/prolog2curry:<version>

where <version> should be something like "1.2.0"
or "latest" to update the latest version.


Description of the Docker image:
--------------------------------

This repository contains a translator which transforms
pure Prolog programs into [Curry](http://curry-lang.org) programs.


How to use the docker image:

In order to translate a pure Prolog program to Curry,
the docker image is simply invoked with passing the Prolog program
on stdin, e.g.,

    > docker run -i --rm currylang/prolog2curry < PROLOGFILE

The informational output can be suppressed with option `-q`:

    > docker run -i --rm currylang/prolog2curry -q < PROLOGFILE

More details from the translation tools can be shown with option `-v3`:

    > docker run -i --rm currylang/prolog2curry -v3 < PROLOGFILE


Details about the transformation implemented by this docker image
can be found in:

M. Hanus: Improving Logic Programs by Adding Functions,
Proc. of the  34th International Symposium on Logic-based Program Synthesis
and Transformation (LOPSTR 2024),
to appear in Springer LNCS, 2024
