# THIS IS A TEMPLATE FOR SUB-PROJECT MAKEFILES
# /Makefile

# subdir level:
#
depth = ..
#

# identify module:
#
NAME = 
#

# descent order into subdirectories:
#
SUBDIRS =
#

# dependencies and linkage of custom libraries:
#

MODULE_LIBES = # $(depth)/lib $(depth)/flower
#

# list of extra distribution files:
# Makefile, C++ and pod are dist'ed automatically
EXTRA_DIST_FILES = 
#

# bootstrap stepmake:
#
include $(depth)/make/stepmake.make 
#

# module compile settings: (not generally needed!)
#
EXTRA_CFLAGS =
EXTRA_CXXFLAGS =
EXTRA_LDFLAGS =
#

# main target of this module:
#
default: $(EXECUTABLE)
# default: $(LIBRARY)
#

# EXECUTABLES=
# include $(stepmake)/Executable.make

