# make/Mutopia.make

FLY_FILES = $(wildcard *.fly)
LY_FILES = $(wildcard *.ly)
M4_FILES = $(wildcard *.m4)
LYM4_FILES = $(wildcard *.lym4)
DIST_FILES = Makefile $(EXTRA_DIST_FILES) $(FLY_FILES) $(LY_FILES) $(M4_FILES) $(LYM4_FILES)
#

include $(depth)/make/Lilypond.make

OUT_FILES = $(addprefix $(outdir)/,$(M4_FILES:%.m4=%)) \
 $(addprefix $(outdir)/,$(LYM4_FILES:%.lym4=%.ly))

all: $(OUT_FILES)

name-stem= $(notdir $(basename $<))

$(outdir)/%.gif: $(outdir)/%.ps
	sh $(buildscripts)/ps-to-gifs.sh $<
	-mv $(name-stem)-page*.gif $(outdir)/
	touch $@

$(outdir)/%.ly.txt: %.ly
	ln -f $< $@

$(outdir)/%.fly.txt: %.fly
	ln -f $< $@

# don't junk intermediate .dvi files.  They're easier to view than
# .ps or .gif
.PRECIOUS: $(outdir)/%.dvi

$(outdir)/%.dvi: %.ly
	sh $(depth)/scripts/ly2dvi.sh -S $(topdir) -o $(outdir)  $< 
	-mv $(basename $<).midi $(outdir)

$(outdir)/%.dvi: %.fly
	sh $(depth)/scripts/ly2dvi.sh -S $(topdir) -o $(outdir)  $< 
	-mv $(basename $<).midi $(outdir)

