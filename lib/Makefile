# subdir level:
#
depth = ..

include ./$(depth)/make/Variables.make 
include ./$(depth)/make/Files.make
include ./$(depth)/make/Version.make 
#

NAME = lily
SUBDIRS = include

MODULE_CXXFLAGS += -D_REENTRANT
MAINTARGET = $(LIBRARY)# huh?

default:  $(LIBRARY)

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make



