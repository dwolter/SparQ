AUTOMAKE_OPTIONS = foreign

SUBDIRS =  Lib Source doc
#csadc CompTable_GPCC gpcc_opra_dipole opracomp2 Qualifier

EXTRA_DIST = Source/*.lisp Source/Makefile Source/copyopralib \
	Calculi/calculus-registry.lisp Calculi/allen.lisp Calculi/cardir.lisp \
	Calculi/absdistcalculus.lisp Calculi/dependency.lisp Calculi/dipole-24.lisp \
	Calculi/dipole-72.lisp Calculi/double-cross.lisp Calculi/double-cross2.lisp Calculi/double-cross2.lisp \
	Calculi/flipflop.lisp Calculi/geometric-orientation.lisp Calculi/point-calculus.lisp \
	Calculi/rcc5.lisp Calculi/rcc8.lisp Calculi/reldistcalculus.lisp Calculi/single-cross.lisp \
	Calculi/opra-calculus.lisp Calculi/qtc-b11.lisp Calculi/qtc-b21.lisp Calculi/qtc-b22.lisp \
	Calculi/qtc-c21.lisp Calculi/qtc-c22.lisp Calculi/block-algebra.lisp\
	Lib/bin sparq Lib/extensions.lisp Lib/qualifiers/*.lisp Lib/translators/*.lisp \
	doc/SparQ-Manual.pdf doc/Makefile \
	README.md INSTALL HISTORY COPYING CREDITS
#	patch-libtool

doc/SparQ-Manual.pdf:
	cd doc; make pdf

sauber: 
	svn status | grep ? | awk '{if (! match($$2,"Makefile") && !match($$2,"configure") && !match($$2,"liboprac")) {print $$2}}' | xargs rm -rf
	mv Makefile.SparQ Makefile

#sparq.py sparq testscript ConstraintReasoning/
