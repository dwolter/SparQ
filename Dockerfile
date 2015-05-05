FROM debian:wheezy
MAINTAINER Hendrik Cech <hendrik.cech@gmail.com>

# make required by SBCL
RUN apt-get update &&\
 apt-get install -y make gcc g++ autotools-dev autoconf automake libtool curl bzip2 &&\
 apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /root

# download and unpack common lisp compiler SBCL
RUN curl http://iweb.dl.sourceforge.net/project/sbcl/sbcl/0.9.10/sbcl-0.9.10-x86-64-linux-binary.tar.bz2 | tar xjf - &&\
 cd sbcl* && GNUMAKE=/usr/bin/make sh install.sh &&\
 cd .. && rm -rf sbcl*

# set sbcl encoding to utf-8
RUN echo "(setf sb-impl::*default-external-format* :UTF-8)" > ~/.sbclrc

ADD . sparq

WORKDIR sparq

# run autotools (and configure)
RUN ln -s /usr/bin/libtoolize /usr/local/bin/glibtoolize &&\
 ./autogen.sh &&\
 rm /usr/local/bin/glibtoolize

# fix
RUN sed -i 's/:save-runtime-options t//' Source/sparq.lisp

# run make and clean up to save space
RUN make &&\
 find ./* -not -name "SparQ.bin"\
 -not -name "sparq"\
 -not -path "./Calculi*"\
 -not -path "./Lib*"\
 -not -name "check.sh"\
 | xargs rm -rf

# precompile
RUN echo "quit" | ./sparq -i

ENTRYPOINT ["/root/sparq/check.sh"]
