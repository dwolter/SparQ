AUTOMAKE_OPTIONS = foreign

#bin_PROGRAMS = 	dipole_qlbe

#dipole_qlbe_SOURCES = dipole_qlbe.cpp

#dipole_qlbe_LDADD = ../qualify-libs/libqualifier.la \
#	../qualify-libs/libqlbeutils.la

# set the include path found by configure
#INCLUDES= $(all_includes)

include_HEADERS = dipole_qlbe.h

INCLUDES = -I@top_srcdir@/Lib/Source/qualify-libs

lib_LTLIBRARIES = libdipole.la
libdipole_la_SOURCES = dipole_qlbe.cpp
#libdipole_la_SOURCES = dipole_qlbe.cpp ../qualify-lib/gpcc.cpp ../qualify-lib/cangle.cpp \
#	../qualify-lib/qualifier.cpp ../qualify-lib/gtpcc.cpp ../qualify-lib/qlbe_utils.cpp

libdipole_la_LIBADD = ../qualify-libs/libqualifier.la ../qualify-libs/libqlbeutils.la


#libdipole_la_LDADD = @top_srcdir@/Lib/Source/qualify-libs/.libs/libqualifier.la \
#	@top_srcdir@/Lib/Source/qualify-libs/.libs/libqlbeutils.la
