# mutopia/Makefile

# subdir level:
#
depth = ..
#

# descent order into subdirectories:
#
SUBDIRS = J.S.Bach
#
# generic stuff/Makefile
#
include ./$(depth)/make/Include.make
# list of distribution files:
# 
LYFILES = $(wildcard *.ly)
TEXFILES = $(wildcard *.tex)
M4FILES = $(wildcard *.m4)
DISTFILES = Makefile TODO $(LYFILES) $(TEXFILES) $(wildcard *.m4)
#

OUTFILES = $(addprefix $(outdir)/,$(M4FILES:%.m4=%))

all: $(OUTFILES)
