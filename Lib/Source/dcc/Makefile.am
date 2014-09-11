AUTOMAKE_OPTIONS = foreign

#bin_PROGRAMS = 	dcc_qlbe

#dcc_qlbe_SOURCES = dcc_qlbe.cpp

#dcc_qlbe_LDADD = ../qualify-libs/libqualifier.la \
#	../qualify-libs/libqlbeutils.la

# set the include path found by configure
#INCLUDES= $(all_includes)

include_HEADERS = dcc_qlbe.h

INCLUDES = -I@top_srcdir@/Lib/Source/qualify-libs

lib_LTLIBRARIES = libdcc.la
libdcc_la_SOURCES = dcc_qlbe.cpp
libdcc_la_LIBADD = ../qualify-libs/libqualifier.la \
	../qualify-libs/libqlbeutils.la


