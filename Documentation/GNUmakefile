depth = ..

NAME = documentation
SUBDIRS=user bibliography pictures topdocs misc windows
STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly
LILYPOND_BOOK_FLAGS=--extra-options '-e "(ly:set-option (quote internal-type-checking) \#t)"'
README_TOP_FILES=NEWS DEDICATION ChangeLog
EXTRA_DIST_FILES=

include $(depth)/make/stepmake.make 

default: local-doc

wiki-dump = $(outdir)/wiki-dump/LilyPond.html
#wiki-slurp: $(wiki-dump)

$(wiki-dump):
	mkdir -p $(outdir)/wiki-dump
	$(PYTHON) $(buildscript-dir)/wiki-slurp.py -d $(outdir)/wiki-dump 'http://lilypond.org/wiki/?' 'http://lilypond.org/wiki/'
foe:
	mkdir -p $(outdir)/wiki-dump/images
	ln -f $(builddir)/Documentation/pictures/$(outbase)/lelie-icon.png $(outdir)/wiki-dump/images/wikibase.png

local-WWW: copy-for-me txt-to-html $(PS_GZ_FILES) $(PDF_FILES) footify

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/wiki-dump

copy-for-me:
	$(foreach a, $(README_TOP_FILES), cp ../$(a) $(outdir)/$(a).txt && ) true

OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(README_TOP_FILES)))

# cd $(outdir) rule gets bit hairy for --srcdir configure builds
txt-to-html:
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)
	$(footify) $(addprefix $(outdir)/, $(addsuffix .html, $(README_TOP_FILES)))
