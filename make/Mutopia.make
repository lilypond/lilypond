# list of distribution files:
# 

include $(depth)/make/Include.make
LYFILES = $(wildcard *.ly)
M4FILES = $(wildcard *.m4)
DISTFILES = Makefile $(EXTRA_DISTFILES) $(LYFILES) $(wildcard *.m4)
#

OUTFILES = $(addprefix $(outdir)/,$(M4FILES:%.m4=%))

all: $(OUTFILES)



name-stem= $(notdir $(basename $<))

$(outdir)/%.gif: $(outdir)/%.ps
	sh $(depth)/bin/ps-to-gifs.sh $<
	mv $(name-stem)-page*.gif $(outdir)/
	touch $@

$(outdir)/%.ly.txt: %.ly
	ln -f $< $@

$(outdir)/%.dvi: %.ly
	ly2dvi -o $(outdir)  $< 

