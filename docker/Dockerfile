FROM ubuntu:24.04

# avoid interactive input for 'configuring tzdata'
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update

RUN apt-get --assume-yes install curl make libnuma1

# required for CPM:
RUN apt-get --assume-yes install git zip unzip sqlite3

# required for KiCS2
RUN apt-get --assume-yes install g++
RUN apt-get --assume-yes install haskell-stack python3 rlwrap
RUN apt-get --assume-yes install gettext # for command envsubst
RUN apt-get --assume-yes install libtinfo-dev # for library libtinfo
    
RUN apt-get --assume-yes install time

RUN rm -rf /var/lib/apt/lists/*

# ensure UTF-8 encoding
ENV LANG C.UTF-8

# copy, unpack and install kics2
WORKDIR /kics2
RUN stack upgrade
RUN curl -sSL -o kics2.tar.gz https://www-ps.informatik.uni-kiel.de/kics2/download/kics2-3.3.0-x86_64-linux-debian11.tar.gz
RUN tar xvzf kics2.tar.gz
RUN rm kics2.tar.gz
RUN mv kics2-3.3.0-x86_64-linux kics2
WORKDIR /kics2/kics2
ENV PATH /kics2/kics2/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN make
RUN make compile-all-libs

ENV HOME /userhome
RUN mkdir -p  $HOME
RUN chmod 777 $HOME

RUN mkdir /kics2/cpm

# options for CPM so that tools will be installed in /kics2/cpm
ENV CPMOPTS="-d REPOSITORY_PATH=/kics2/cpm/index -d PACKAGE_INSTALL_PATH=/kics2/cpm/packages -d APP_PACKAGE_PATH=/kics2/cpm/app_packages -d BIN_INSTALL_PATH=/kics2/cpm/bin"

RUN bin/cypm $CPMOPTS update
RUN bin/cypm $CPMOPTS install prolog2curry
RUN bin/cypm $CPMOPTS install verify-non-fail

# set path with cpm bin directory included
ENV PATH /kics2/cpm/bin:/kics2/kics2/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# make tool files readable
RUN chmod -R go+rX /kics2/cpm/app_packages

# Maybe install Z3?

# Initialize nonfailure verification tool by analyzing the prelude:
RUN /kics2/cpm/bin/curry-calltypes Prelude

# copy script to transform Prolog programs:
COPY pl2curry-failsensitive.sh /kics2/kics2/bin/pl2curry
RUN chmod 755 /kics2/kics2/bin/pl2curry

# set transformation script as entrypoint
ENTRYPOINT ["/kics2/kics2/bin/pl2curry"]
