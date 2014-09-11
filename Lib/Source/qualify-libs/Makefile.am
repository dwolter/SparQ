#bin_PROGRAMS = qualifier
#qualifier_SOURCES = qualifier.cpp gpcc.cpp  cangle.cpp \
#			 main.cpp gtpcc.cpp 

# set the include path found by configure
INCLUDES= $(all_includes)

# the library search path.
#qualifier_LDFLAGS = $(all_libraries) 
#noinst_HEADERS = qualifier.h 
include_HEADERS = qualifier.h cangle.h gpcc.h gtpcc.h \
	 point.h vector.h geometry.h line.h qlbe_utils.h

noinst_LTLIBRARIES = libqualifier.la libqlbeutils.la
libqualifier_la_SOURCES = gpcc.cpp cangle.cpp \
		qualifier.cpp gtpcc.cpp 
libqlbeutils_la_SOURCES = qlbe_utils.cpp
