# Files.make


# list of object _FILES:
#
SOURCE_FILES =

O_FILES = $(addprefix $(outdir)/,$(OBJECT_FILES))
#

DEP_FILES := $(wildcard $(depdir)/*.dep)

# (Why not Makefile too?)
#
MAKE_FILES := $(wildcard *.make)
#

IN_FILES := $(wildcard *.in)
SOURCE_FILES += $(IN_FILES)
SOURCE_FILES += $(MAKE_FILES)
# Preprocessed .in documentation _FILES:
#
OUTIN_FILES = $(addprefix $(outdir)/, $(IN_FILES:%.in=%))
#

ALL_SOURCES = $(SOURCE_FILES)

