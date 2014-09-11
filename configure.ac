AC_INIT(Lib/Source/opra/OpraOnTheFly.cpp)

printf "checking for sbcl... "
which sbcl > /dev/null
if test $? -eq 0
  then
    printf " OK\nchecking SBCL version... "
    sbcl --noinform --eval "(progn (print *features*) (quit))" | grep :DARWIN > /dev/null
    if test $? -eq 0
      then
        # we're running on a Mac: check whether 32bit or 64bit...
        sbcl --noinform --eval "(progn (print *features*) (quit))" | grep :X86-64 > /dev/null
        if test $? -eq 0
          then    
	    CXXFLAGS="-m64 $CXXFLAGS"
	    printf "OK - 64 bit\n"
          else
            CXXFLAGS="-m32 $CXXFLAGS"
	    printf "OK - 32 bit\n"
        fi
      else
        # we're running on 
        printf "OK"

    fi
  else
    printf "NO\nERROR: No sbcl found in path\n"
    exit 1
fi


AM_INIT_AUTOMAKE(sparq,0.8.0)

AM_PROG_LIBTOOL

AM_CONFIG_HEADER(config.h)

AC_LANG_C

AC_PROG_CC

AC_PROG_CXX

#AC_PROG_RANLIB

#AC_PROG_INSTALL

AC_DEFUN([AC_DATAROOTDIR_CHECKED])

AC_CHECK_LIB(m, cos, LIBM="-lm")

AC_SUBST(DEPS_CFLAGS)
AC_SUBST(DEPS_LIBS)

                   
AC_OUTPUT(	\
		Makefile \
		Lib/Makefile \
		Lib/Source/Makefile \
		Lib/Source/opra/Makefile \
		Lib/Source/dipole/Makefile \
		Lib/Source/scc/Makefile \
		Lib/Source/dcc/Makefile \
		Lib/Source/dcc2/Makefile \
		Lib/Source/tpcc/Makefile \
		Lib/Source/qualify-libs/Makefile \
		Lib/Source/qtc/Makefile
)
#		csadc/Makefile \
#		CompTable_GPCC/Makefile \
#		gpcc_opra_dipole/Makefile		
#		opracomp2/Makefile	\
#		Qualifier/Makefile
#)

