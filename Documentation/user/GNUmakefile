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
LOCALSTEPMAKE_TEMPLATES=lilypond mudela

include $(depth)/make/stepmake.make 

dvi: $(DVI_FILES)

ps: $(PS_FILES)

info: $(INFO_FILES)

default: 

local-WWW: $(HTML_FILES)   $(datafiles) $(PS_GZ_FILES) backdoc-WWW
	$(SHELL) $(buildscript-dir)/install-info-html.sh $(outdir) lilypond lilypond-internals
	$(MAKE) footify
	ln -f $(outdir)/*.png $(outdir)/lilypond
	$(MAKE) deep-footify

deep-footify:
	$(PYTHON) $(step-bindir)/add-html-footer.py --index=../../$(depth)/ --name $(PACKAGE_NAME) --version $(TOPLEVEL_VERSION) --header=$(depth)/Documentation/header.html.in --footer $(depth)/Documentation/footer.html.in $(sort $(wildcard $(outdir)/*/*.html))


#	$(PYTHON) $(step-bindir)/ls-latex.py --title 'User documentation' \
#	   $(DOC_FILES) $(TEX_FILES) $(TELY_FILES) \
#	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html

$(outdir)/%.bib: %.bib
	ln -f $< $@
# we want footers even if website builds (or is built) partly
	$(MAKE) footify

localclean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk 

backdoc-WWW: $(outdir)/lilypond-internals.html
	$(MAKE) footify

#ugh. lily/OUT/lilypond hardcoded.
# when cross-compiling, we don't have lilypond
$(outdir)/lilypond-internals.texi:
	if [ -e $(depth)/$(builddir)/lily/out/lilypond ]; then \
		cd $(outdir) && ../$(depth)/$(builddir)/lily/out/lilypond ../$(src-depth)/ly/generate-documentation; \
	else \
		touch $@; \
	fi
