# Files.make


O_FILES = $(addprefix $(outdir)/,$(OBJECT_FILES))

DEP_FILES := $(wildcard $(outdir)/*.dep)

# (Why not Makefile too?)
#
#

IN_FILES := $(wildcard *.in)
SOURCE_FILES += $(IN_FILES)

# Preprocessed .in documentation _FILES:
#
OUTIN_FILES = $(addprefix $(outdir)/, $(IN_FILES:%.in=%))
#

ALL_SOURCES = $(SOURCE_FILES)

