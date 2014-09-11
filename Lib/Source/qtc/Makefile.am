AUTOMAKE_OPTIONS = foreign

#bin_PROGRAMS = qtccomposition qtc_qlbe
#bin_PROGRAMS = qtccomposition
#qtccomposition_SOURCES = QTC.cpp
#qtccomposition_LDADD =  libqtcc.la 

#qtc_qlbe_SOURCES = qtc_qlbe.cpp
#qtc_qlbe_LDADD = ../qualify-libs/libqualifier.la \
#	../qualify-libs/libqlbeutils.la

# set the include path found by configure
INCLUDES = -I@top_srcdir@/Lib/Source/qualify-libs

#include_HEADERS = QTC.h Misc.h Interface.h QTC_qlbe.h
include_HEADERS = QTC.h Misc.h Interface.h 

# the library search path.
#qtccomposition_LDFLAGS = $(all_libraries)

lib_LTLIBRARIES = libqtcc.la
#libqtcc_la_SOURCES = Interface.cpp QTC.cpp Misc.cpp opra_qlbe.cpp
libqtcc_la_SOURCES = Interface.cpp QTC.cpp Misc.cpp

#libqtcc_la_LIBADD = ../qualify-libs/libqualifier.la ../qualify-libs/libqlbeutils.la
libqtcc_la_LIBADD = ../qualify-libs/libqualifier.la
