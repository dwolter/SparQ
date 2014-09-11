AUTOMAKE_OPTIONS = foreign

#bin_PROGRAMS = opracomposition opra_qlbe
#opracomposition_SOURCES = Opracomposition.cpp
#opracomposition_LDADD =  liboprac.la 

#opra_qlbe_SOURCES = opra_qlbe.cpp
#opra_qlbe_LDADD = ../qualify-libs/libqualifier.la \
#	../qualify-libs/libqlbeutils.la

# set the include path found by configure
INCLUDES = -I@top_srcdir@/Lib/Source/qualify-libs

include_HEADERS = Configuration.h OpraOnTheFly.h Misc.h Interface.h opra_qlbe.h

# the library search path.
#opracomposition_LDFLAGS = $(all_libraries)

lib_LTLIBRARIES = liboprac.la
liboprac_la_SOURCES = Interface.cpp OpraOnTheFly.cpp Configuration.cpp Misc.cpp opra_qlbe.cpp

liboprac_la_LIBADD = ../qualify-libs/libqualifier.la ../qualify-libs/libqlbeutils.la
