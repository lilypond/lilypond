#!/usr/bin/make
# Mutopia Makefile Project
#

#
# For testing installed lily
#
make-root=$(wildcard $(HOME)/tmp/test/usr/share/lilypond/make)
#make-root=$(wildcard $(HOME)/usr/src/lilypond/make)
make-root?=$(wildcard /usr/share/lilypond/make)

ifneq ($(make-root),)

depth=$(make-root)/..
LOCALSTEPMAKE_TEMPLATES=mutopia
include $(make-root)/stepmake.make

else
	burp
#
# Minimalistic standalone part.  I'd very much like to install make/*
# and stepmake/stepmake/* in /usr/share/lilypond, and junk this.
#

outdir=out
PAPERSIZE=
LY_FILES=$(wildcard *.ly)

default:
	echo $(DEP_FILES)
	$(examples)
clean:
	rm -f $(outdir)/* $(outdir)-letter/*


#
# stepmake/stepmake/files.ly
#
DEP_FILES:=$(wildcard $(outdir)/*.dep)

#
# stepmake/stepmake/generic-targets.make
#
include $(outdir)/dummy.dep $(DEP_FILES)

$(outdir)/dummy.dep:
	-mkdir -p $(outdir)
	touch $(outdir)/dummy.dep

#
# make/mutopia-rules.make
#

# don't junk intermediate .dvi files.  They're easier to view than
# .ps or .png
.PRECIOUS: $(outdir)/%.dvi

#
# should we dist ps-to-pngs?
#
$(outdir)/%.png: $(outdir)/%.ps
	ps-to-pngs $<
	-mv $(name-stem)-page*.png $(outdir)/
	touch $@

$(outdir)/%.dvi: %.ly
	ly2dvi --outdir=$(outdir) --dependencies $< 
	-mv $(basename $<)*.midi $(outdir)

$(outdir)-$(PAPERSIZE)/%.dvi: %.ly
	ly2dvi.py --outdir=$(outdir)-$(PAPERSIZE) --dependencies --papersize=$(PAPERSIZE) $< 
	-mv $(basename $<)*.midi $(outdir)-$(PAPERSIZE)


#
# stepmake/stepmake/tex-rules.make
#
$(outdir)/%.tex: %.tex
	cp $< $@

$(outdir)/%.dvi: $(outdir)/%.tex
	(cd $(outdir); tex \\nonstopmode \\input $(<F))

$(outdir)/%.dvi: $(outdir)/%.latex
	(cd $(outdir)&& \
	  latex \\nonstopmode \\input $(<F)&&\
	  (bibtex $(basename $(<F)) || true) && \
	  latex \\nonstopmode \\input $(<F)&&\
	  (makeindex $(basename $(<F)) || true) && \
	  latex \\nonstopmode \\input $(<F) )

$(outdir)/%.ps: $(outdir)/%.dvi
	cd $(outdir) && dvips -ta4 -o $(@F) $(<F)

$(outdir)-$(PAPERSIZE)/%.ps: $(outdir)-$(PAPERSIZE)/%.dvi
	cd $(outdir) && dvips -t$(PAPERSIZE) -o $(@F) $(<F)


#
# make/ly-rules.make
#

$(outdir)/%.latex: %.doc
	rm -f $@
	lilypond-book --outdir=$(outdir) --dependencies $<
	chmod -w $@

endif


#
# Mutopia/user targets.
# This needs some work.
#


parts=$(patsubst %.ly,%,$(wildcard *-part.ly))

tarball=coriolan
mutopia-examples=coriolan $(parts)
mutopia-letter=$(mutopia-examples:%=out-letter/%.ps.gz)


mutopia:
	$(MAKE) examples="$(mutopia-examples)" PAPERSIZE=letter local-WWW $(mutopia-letter)

