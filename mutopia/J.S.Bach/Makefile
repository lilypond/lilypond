# mutopia/J.S.Bach/Makefile

# subdir level:
#
depth = ../..
#

# descent order into subdirectories:
#
SUBDIRS = 
# 

# generic stuff/Makefile
#
include ./$(depth)/make/Include.make
#
#

# list of distribution files:
# 
LYFILES = $(wildcard *.ly)
TEXFILES = $(wildcard *.tex)
M4FILES = $(wildcard *.m4)
DISTFILES = Makefile $(LYFILES) $(TEXFILES) $(M4FILES)
#

OUTFILES = $(addprefix $(outdir)/,$(M4FILES:%.m4=%))

all: $(OUTFILES)
