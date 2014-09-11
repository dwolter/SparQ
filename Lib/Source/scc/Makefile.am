AUTOMAKE_OPTIONS = foreign

#bin_PROGRAMS = 	scc_qualifier

#scc_qualifier_SOURCES = scc_qualify.h

#scc_qualifier_LDADD = ../qualify-libs/libqualifier.la \
#	../qualify-libs/libqlbeutils.la

# set the include path found by configure
#INCLUDES= $(all_includes)

include_HEADERS = scc_qualify.h

INCLUDES = -I@top_srcdir@/Lib/Source/qualify-libs

lib_LTLIBRARIES = libscc.la
libscc_la_SOURCES = scc_qualify.cpp
#libdipole_la_SOURCES = dipole_qlbe.cpp ../qualify-lib/gpcc.cpp ../qualify-lib/cangle.cpp \
#	../qualify-lib/qualifier.cpp ../qualify-lib/gtpcc.cpp ../qualify-lib/qlbe_utils.cpp

libscc_la_LIBADD = ../qualify-libs/libqualifier.la 
#../qualify-libs/libqlbeutils.la


#libdipole_la_LDADD = @top_srcdir@/Lib/Source/qualify-libs/.libs/libqualifier.la \
#	@top_srcdir@/Lib/Source/qualify-libs/.libs/libqlbeutils.la
