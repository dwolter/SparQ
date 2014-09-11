AUTOMAKE_OPTIONS = foreign

#bin_PROGRAMS = 	tpcc_qualifier

#tpcc_qualifier_SOURCES = tpcc_qualify.h

#tpcc_qualifier_LDADD = ../qualify-libs/libqualifier.la \
#	../qualify-libs/libqlbeutils.la

# set the include path found by configure
#INCLUDES= $(all_includes)

include_HEADERS = tpcc_qualify.h

INCLUDES = -I@top_srcdir@/Lib/Source/qualify-libs

lib_LTLIBRARIES = libtpcc.la
libtpcc_la_SOURCES = tpcc_qualify.cpp
#libtpcc_la_SOURCES = tpcc_qlbe.cpp ../qualify-lib/gpcc.cpp ../qualify-lib/cangle.cpp \
#	../qualify-lib/qualifier.cpp ../qualify-lib/gtpcc.cpp ../qualify-lib/qlbe_utils.cpp

libtpcc_la_LIBADD = ../qualify-libs/libqualifier.la 
#../qualify-libs/libqlbeutils.la


#libdipole_la_LDADD = @top_srcdir@/Lib/Source/qualify-libs/.libs/libqualifier.la \
#	@top_srcdir@/Lib/Source/qualify-libs/.libs/libqlbeutils.la
