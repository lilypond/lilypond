# list of distribution files:
# 

include $(depth)/make/Include.make
LYFILES = $(wildcard *.ly)
TEXFILES = $(wildcard *.tex)
M4FILES = $(wildcard *.m4)
DISTFILES = Makefile $(LYFILES) $(TEXFILES) $(wildcard *.m4)
#

OUTFILES = $(addprefix $(outdir)/,$(M4FILES:%.m4=%))

all: $(OUTFILES)

