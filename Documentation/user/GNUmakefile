# Documentation/tex/Makefile

depth=../..



LATEX_FILES =$(wildcard *.latex)


# todo: add latex.
DVI_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.dvi))

EXTRA_DIST_FILES= $(LATEX_FILES)    $(wildcard *.itexi *.itely)

HTML_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.html))

PS_FILES = $(DVI_FILES:.dvi=.ps)  $(OUTDOC_FILES:.doc=.ps) $(OUTTEX_FILES:.tex=.ps)

PS_GZ_FILES= $(addsuffix .gz, $(PS_FILES))

INFO_FILES = $(addprefix $(outdir)/, lilypond.info lilypond-internals.info)

STEPMAKE_TEMPLATES=tex texinfo documentation
LOCALSTEPMAKE_TEMPLATES=lilypond ly

include $(depth)/make/stepmake.make 

dvi: $(DVI_FILES)

ps: $(PS_FILES)

# Cancel default info rule
$(outdir)/%.info: $(outdir)/%.texi

default: 

# info is now built by default via texinfo-rules
# we must build them by default, otherwise they get built during make install
info: $(INFO_FILES)


DEEP_HTML_FILES = $(outdir)/lilypond/lilypond.html $(outdir)/lilypond-internals/lilypond-internals.html

local-WWW: $(HTML_FILES) $(datafiles) $(PS_GZ_FILES) $(DEEP_HTML_FILES)
	$(SHELL) $(buildscript-dir)/install-info-html.sh --dir=$(outdir) lilypond lilypond-internals
	$(MAKE) footify
	-mkdir $(outdir)/lilypond 
	cp -f $(outdir)/*.png $(outdir)/lilypond
	$(MAKE) deep-footify

#	$(PYTHON) $(step-bindir)/ls-latex.py --title 'User documentation' \
#	   $(DOC_FILES) $(TEX_FILES) $(TELY_FILES) \
#	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html

$(outdir)/%.bib: %.bib
	ln -f $< $@
# we want footers even if website builds (or is built) partly
	$(MAKE) footify

localclean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk 

backdoc-WWW: $(outdir)/lilypond-internals/lilypond-internals.html
	$(SHELL) $(buildscript-dir)/install-info-html.sh --dir=$(outdir) lilypond lilypond-internals

#ugh. lily/OUT/lilypond hardcoded.
# when cross-compiling, we don't have lilypond
ifneq ($(CROSS),yes)
$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi:
	cd $(outdir) && ../$(depth)/$(builddir)/lily/out/lilypond ../$(src-depth)/ly/generate-documentation
	-ln $(outdir)/lilypond-internals.texi $(outdir)/lilypond-internals.nexi
else
$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi:
##	-cd $(outdir) && lilypond ../$(src-depth)/ly/generate-documentation
##programming error: Improbable offset for translation: setting to zero (Continuing; cross thumbs)
##programming error: Molecule::add_at_edge: adding empty molecule. (Continuing; cross thumbs)
	touch $@
	touch $(outdir)/$(*F).nexi
endif

# Generic rule not possible?
# $(outdir)/%/%.html: $(outdir)/%.texi
$(outdir)/lilypond/lilypond.html: $(outdir)/lilypond.texi
	-$(MAKEINFO) --force --output=$@ --html $<
# we want footers even if website builds (or is built) partly
	$(deep-footify) $(sort $(wildcard $(outdir)/$(*F)/*.html))


$(outdir)/lilypond-internals/lilypond-internals.html: $(outdir)/lilypond-internals.texi
	-$(MAKEINFO) --force --output=$@ --html $<
# we want footers even if website builds (or is built) partly
	$(deep-footify) $(sort $(wildcard $(outdir)/$(*F)/*.html))


