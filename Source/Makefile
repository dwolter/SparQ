# This file is part of SparQ, a toolbox for qualitative spatial reasoning.
# Copyright (C) 2006-2014 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
# See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

# SparQ is free software and has been released under the terms of the GNU
# General Public License version 3 or later. You should have received a
# copy of the GNU General Public License along with this program. If not,
# see <http://www.gnu.org/licenses/>.

LISP = sbcl
OPRADIR = ../Lib/Source/opra/.libs
QTCDIR = ../Lib/Source/qtc/.libs
BINDIR = ../Lib/bin
SH = /bin/sh

##
## Compile the SparQ binary from the lisp source
##

lispbin: calculi.lisp constraint-reasoning.lisp main.lisp compute-relation.lisp \
	 sparq.lisp  qualifier.lisp relations.lisp
	$(LISP) --noinform --load "sparq.lisp" --eval "(progn (sparq:make-sparq))"
	mv sparq ..

##
## Build the OPRA compositon library
## (other external composition librarys are handled here as well)
##

libopra:
	$(SH) ./copyopralib


all: libopra lispbin

##
## Remove compilation output and temp files
##
clean:
#	cd ../Lib ; make clean
	rm -f ../SparQ.bin
	rm -f SparQ.bin
	rm -f *.fasl
	rm -f ../Calculi/*.fasl
	rm -f *.dfsl
	rm -f ../Calculi/*.dfsl
	rm -f ../Lib/bin/*

distdir: 

dist:

distclean: clean

