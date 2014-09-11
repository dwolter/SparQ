AUTOMAKE_OPTIONS = foreign

#bin_PROGRAMS = 	dcc_qlbe

#dcc_qlbe_SOURCES = dcc_qlbe.cpp

#dcc_qlbe_LDADD = ../qualify-libs/libqualifier.la \
#	../qualify-libs/libqlbeutils.la

# set the include path found by configure
#INCLUDES= $(all_includes)

include_HEADERS = dcc2_qlbe.h

INCLUDES = -I@top_srcdir@/Lib/Source/qualify-libs

lib_LTLIBRARIES = libdcc2.la
libdcc2_la_SOURCES = dcc2_qlbe.cpp
libdcc2_la_LIBADD = ../qualify-libs/libqualifier.la \
	../qualify-libs/libqlbeutils.la


