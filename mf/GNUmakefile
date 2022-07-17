# mf/GNUmakefile

depth = ..

TEMPLATES = install install-out lilypond

FETA_MF_FILES = \
  $(call src-wildcard,feta[0-9]*.mf) \
  $(call src-wildcard,feta-braces-[a-z].mf) \
  $(call src-wildcard,feta-alphabet*[0-9].mf) \
  $(call src-wildcard,feta-noteheads*[0-9].mf) \
  $(call src-wildcard,feta-flags*[0-9].mf) \
  $(call src-wildcard,parmesan[0-9]*.mf) \
  $(call src-wildcard,parmesan-noteheads*[0-9].mf)
FETA_FONTS = $(FETA_MF_FILES:.mf=)
PFB_FILES = $(FETA_FONTS:%=$(outdir)/%.pfb)

include $(depth)/make/lilypond.make

# Don't remove $(outdir)/.log's.  Log files are a target!
# We want to see botched results as well.
# TODO: use the dependency scanning from invoke-mf2pt1.sh here too.
$(outdir)/%.dvi: %.mf
	$(call ly_progress,Making,$@,< mf)
	MFINPUTS=$(src-dir) \
	  max_print_line=1000 \
	  $(METAFONT) "\scrollmode; input $<;" $(METAFONT_QUIET)
	gftodvi $(basename $<)
	mv $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.tfm \
$(outdir)/%.log \
$(outdir)/%.pfb: \
  %.mf \
  $(outdir)/mf2pt1.mem invoke-mf2pt1.sh
	$(call ly_progress,Making,$(outdir)/$*.pfb,< mf)
	FONTFORGE="$(FONTFORGE) $(FONTFORGE_QUIET)" \
	  $(src-dir)/invoke-mf2pt1.sh \
	    "$(PERL) $(buildscript-dir)/mf2pt1.pl" \
	    $< $(outdir)/$*.pfb $(METAFONT_QUIET)

# Since recent mpost versions no longer use a mem file, we create a dummy
# file to satisfy the dependency (which gets overwritten in case an older
# mpost creates a real mem file).
$(outdir)/mf2pt1.mem: mf2pt1.mp
	$(call ly_progress,Making,$@,< mp)
	cd $(outdir) \
	  && touch mf2pt1.mem \
	  && mpost -progname=mpost \
	           -ini $(top-src-dir)/mf/mf2pt1.mp \\dump $(METAFONT_QUIET)

ifndef VERBOSE
  METAFONT_QUIET = >/dev/null
  FONTFORGE_QUIET = $(FONTFORGE_QUIET_OPTION)
else
  METAFONT_QUIET =
  FONTFORGE_QUIET =
endif

$(outdir)/emmentaler-brace.subfonts:
	$(call ly_progress,Making,$@,)
	echo $(subst .mf,,$(call src-wildcard,feta-braces-[a-z].mf)) > $@

STAFF_SIZES = 11 13 14 16 18 20 23 26
BRACES = a b c d e f g h i

OTF_FILES = \
  $(STAFF_SIZES:%=$(outdir)/emmentaler-%.otf) \
  $(outdir)/emmentaler-brace.otf
SVG_FILES = $(OTF_FILES:%.otf=%.svg)
WOFF_FILES = $(OTF_FILES:%.otf=%.woff)

LOG_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.log)

ALL_GEN_FILES = \
  $(OTF_FILES) \
  $(SVG_FILES) \
  $(WOFF_FILES)

default: $(ALL_GEN_FILES)

INSTALLATION_SUFFIXES = 1

INSTALLATION_DIR = $(local_lilypond_datadir)/fonts/source
INSTALLATION_FILES = $(call src-wildcard,*.mf)

INSTALLATION_DIR1 = $(local_lilypond_datadir)/fonts
INSTALLATION_FILES1 = $(call src-wildcard,*.conf)

INSTALLATION_OUT_SUFFIXES = 1 2

INSTALLATION_OUT_DIR1 = $(local_lilypond_datadir)/fonts/otf
INSTALLATION_OUT_FILES1 = $(OTF_FILES)

INSTALLATION_OUT_DIR2 = $(local_lilypond_datadir)/fonts/svg
INSTALLATION_OUT_FILES2 = \
  $(SVG_FILES) \
  $(WOFF_FILES)


$(outdir)/%.global-lisp \
$(outdir)/%.lisp: $(outdir)/%.log
	$(call ly_progress,Making,$(outdir)/$*.lisp,< log)
	$(PYTHON) $(buildscript-dir)/mf-to-table.py $<

$(outdir)/emmentaler-%.svg \
$(outdir)/emmentaler-%.woff \
$(outdir)/emmentaler-%.otf: \
  gen-emmentaler.fontforge.py \
  emmentaler_codes.py \
  emmentaler_features.py \
  emmentaler_kerning.py \
  \
  $(outdir)/feta%.pfb $(outdir)/feta%.lisp \
  $(outdir)/feta-alphabet%.pfb $(outdir)/feta-alphabet%.lisp \
  $(outdir)/feta-flags%.pfb $(outdir)/feta-flags%.lisp \
  $(outdir)/feta-noteheads%.pfb $(outdir)/feta-noteheads%.lisp \
  $(outdir)/parmesan%.pfb $(outdir)/parmesan%.lisp \
  $(outdir)/parmesan-noteheads%.pfb $(outdir)/parmesan-noteheads%.lisp \
  \
  $(outdir)/feta%.global-lisp \
  $(outdir)/feta-alphabet%.tfm
	$(call ly_progress,Making,$@,)
	$(FONTFORGE) \
	  $(FONTFORGE_QUIET) \
	  -lang=py \
	  -script $< \
	  --version=$(VERSION) \
	  --in $(outdir)/ \
	  --out $(outdir)/emmentaler-$*.otf

$(outdir)/emmentaler-brace.otf: \
  gen-emmentaler-brace.fontforge.py \
  $(foreach s,$(BRACES),$(outdir)/feta-braces-$(s).pfb) \
  $(foreach s,$(BRACES),$(outdir)/feta-braces-$(s).lisp)
	$(call ly_progress,Making,$@,)
	$(FONTFORGE) \
	  $(FONTFORGE_QUIET) \
	  -lang=py \
	  -script $< \
	  --version $(VERSION) \
	  --in $(outdir) \
	  --out=$(outdir)/emmentaler-brace.otf
	touch \
	  $@ \
	  $(outdir)/emmentaler-brace.woff \
	  $(outdir)/emmentaler-brace.svg

$(outdir)/emmentaler-brace.woff \
$(outdir)/emmentaler-brace.svg: $(outdir)/emmentaler-brace.otf
	@true
