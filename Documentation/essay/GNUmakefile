depth = ../..

STEPMAKE_TEMPLATES=tex documentation
LOCALSTEPMAKE_TEMPLATES = lilypond ly

OUT_BIB_FILES = $(addprefix $(outdir)/, $(BIB_FILES))
EXTRA_DIST_FILES= $(BIB_FILES) $(call src-wildcard,*.bst) 
BIB_FILES= $(call src-wildcard,*.bib)

include $(depth)/make/stepmake.make

# we no longer create bibliography files in this subdir,
# since it needs to be done in the main Documentation/ dir.
# all the below can _probably_ be removed, but I haven't
# examined it in detail.
# I think this is just going to wait until we switch to waf.  -gp
dvi: $(DVI_FILES) $(OUT_BIB_FILES)

ps: $(PS_FILES)

default:

GENHTMLS = engraving colorado computer-notation
OUTGENHTMLS = $(addprefix $(outdir)/, $(GENHTMLS:%=%.html))

ifeq ($(out),www)
#   temporary build fix; we don't know yet what's happening
#   to the literature index.  -gp
# local-WWW-2: $(addprefix $(outdir)/, $(BIB_FILES:.bib=.html) lit-index.html)
endif

$(outdir)/%.bib: %.bib
	ln -f $< $@

$(outdir)/%.html: %.bib
	BSTINPUTS=$(src-dir) $(buildscript-dir)/bib2html -o $@ $<

local-clean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk

