# mf/GNUmakefile

depth = ..

STEPMAKE_TEMPLATES = install install-out
LOCALSTEPMAKE_TEMPLATES = lilypond

# These are the main .mf files.  We don't use $(MF_FILES) here,
# because there are more .mf files, input'ed into the main files.
FETA_MF_FILES = $(call src-wildcard,feta[0-9]*.mf) \
		$(call src-wildcard,feta-braces-[a-z].mf) \
		$(call src-wildcard,feta-alphabet*[0-9].mf) \
		$(call src-wildcard,feta-noteheads*[0-9].mf) \
		$(call src-wildcard,feta-flags*[0-9].mf) \
		$(call src-wildcard,parmesan[0-9]*.mf) \
		$(call src-wildcard,parmesan-noteheads*[0-9].mf)
FETA_FONTS = $(FETA_MF_FILES:.mf=)
ALL_FONTS = $(FETA_FONTS)
PFB_FILES = $(ALL_FONTS:%=$(outdir)/%.pfb)

include $(depth)/make/stepmake.make

# Don't remove $(outdir)/.log's.  Logs are a target!
# we want to see botched results as well.
# TODO: use the dependency scanning from invoke-mf2pt1.sh here too.
$(outdir)/%.dvi: %.mf
	$(call ly_progress,Making,$@,< mf)
	MFINPUTS=$(src-dir) \
	  max_print_line=1000 \
	  $(METAFONT) "\scrollmode; input $<;" $(METAFONT_QUIET)
	gftodvi $(basename $<)
	mv $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.tfm $(outdir)/%.log $(outdir)/%.pfb: %.mf $(outdir)/mf2pt1.mem invoke-mf2pt1.sh
	$(call ly_progress,Making,$(outdir)/$*.pfb,< mf)
	$(src-dir)/invoke-mf2pt1.sh $(buildscript-dir)/mf2pt1 $< $(outdir)/$*.pfb $(METAFONT_QUIET)

# since recent mpost versions no longer create a mem file, we create a dummy
# file to satisfy the dependency (which gets overwritten in case an older
# mpost creates a real mem file)
$(outdir)/mf2pt1.mem: mf2pt1.mp
	$(call ly_progress,Making,$@,< mp)
	cd $(outdir) \
	   && touch mf2pt1.mem \
	   && mpost -progname=mpost -ini $(top-src-dir)/mf/mf2pt1.mp \\dump $(METAFONT_QUIET)

ifndef VERBOSE
METAFONT_QUIET = >/dev/null
else
METAFONT_QUIET =
endif

$(outdir)/emmentaler-brace.subfonts:
	$(call ly_progress,Making,$@,)
	echo $(subst .mf,,$(call src-wildcard,feta-braces-[a-z].mf)) > $@

$(outdir)/emmentaler-%.genpe: $(buildscript-dir)/gen-emmentaler-scripts
	$(call ly_progress,Making,$@,)
	$< --dir=$(outdir) --design-size=$(patsubst emmentaler-%.genpe,%,$(notdir $@))


EXTRA_DIST_FILES += README mf2pt1.mp invoke-mf2pt.sh

STAFF_SIZES = 11 13 14 16 18 20 23 26
BRACES = a b c d e f g h i

OTF_FILES = $(STAFF_SIZES:%=$(outdir)/emmentaler-%.otf) \
           $(outdir)/emmentaler-brace.otf
SVG_FILES = $(OTF_FILES:%.otf=%.svg)
WOFF_FILES = $(OTF_FILES:%.otf=%.woff)

TEXGYRE_OTFS = $(addprefix $(TEXGYRE_DIR)/,$(TEXGYRE_FILES))
URWOTF_OTFS = $(addprefix $(URWOTF_DIR)/,$(URWOTF_FILES))

LILYPOND_FONTS_CONF = $(outdir)/00-lilypond-fonts.conf \
		      $(outdir)/99-lilypond-fonts.conf

LOG_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.log)

ALL_GEN_FILES = $(OTF_FILES) \
		$(SVG_FILES) \
		$(WOFF_FILES) \
		$(LILYPOND_FONTS_CONF)

INSTALLATION_DIR = $(local_lilypond_datadir)/fonts/source
INSTALLATION_FILES = $(call src-wildcard,*.mf)

INSTALLATION_OUT_SUFFIXES = 1 2 3

INSTALLATION_OUT_DIR1 = $(local_lilypond_datadir)/fonts/otf
INSTALLATION_OUT_FILES1 = $(OTF_FILES) \
			  $(TEXGYRE_OTFS) \
			  $(URWOTF_OTFS)

INSTALLATION_OUT_DIR2 = $(local_lilypond_datadir)/fonts/svg
INSTALLATION_OUT_FILES2 = $(SVG_FILES) $(WOFF_FILES)

INSTALLATION_OUT_DIR3 = $(local_lilypond_datadir)/fonts
INSTALLATION_OUT_FILES3 = $(LILYPOND_FONTS_CONF)


# only for fonts which
#
# 1. are mentioned in font.scm
#
# 2. are not included with teTeX
#
$(outdir)/%.global-lisp $(outdir)/%.lisp:  $(outdir)/%.log
	$(call ly_progress,Making,$(outdir)/$*.lisp,< log)
	$(buildscript-dir)/mf-to-table $<

$(outdir)/emmentaler-%.svg $(outdir)/emmentaler-%.woff $(outdir)/emmentaler-%.otf: gen-emmentaler.fontforge.py\
			$(outdir)/feta%.pfb \
			    $(outdir)/feta-noteheads%.pfb \
			    $(outdir)/feta-flags%.pfb \
			    $(outdir)/feta-alphabet%.pfb \
			    $(outdir)/parmesan%.pfb \
			    $(outdir)/parmesan-noteheads%.pfb \
			    $(outdir)/feta%.lisp \
			    $(outdir)/feta-noteheads%.lisp \
			    $(outdir)/feta-flags%.lisp \
			    $(outdir)/feta-alphabet%.lisp \
			    $(outdir)/parmesan%.lisp \
			    $(outdir)/parmesan-noteheads%.lisp \
			    $(outdir)/feta%.global-lisp \
			    $(outdir)/feta-alphabet%.tfm
	$(call ly_progress,Making,$@,)
	$(FONTFORGE) -lang=py -script $< --version=$(VERSION) --in $(outdir)/ --out $(outdir)/emmentaler-$*.otf


$(outdir)/emmentaler-brace.otf: gen-emmentaler-brace.fontforge.py \
		$(foreach s,$(BRACES),$(outdir)/feta-braces-$(s).pfb) \
		$(foreach s,$(BRACES),$(outdir)/feta-braces-$(s).lisp)
	$(call ly_progress,Making,$@,)
	$(FONTFORGE) -lang=py -script $< --version $(VERSION) --out=$(outdir)/emmentaler-brace.otf --in $(outdir)
	touch $@ $(outdir)/emmentaler-brace.woff $(outdir)/emmentaler-brace.svg

$(outdir)/emmentaler-brace.woff $(outdir)/emmentaler-brace.svg: $(outdir)/emmentaler-brace.otf
	@true

default: tree-regen $(outdir)/fonts.conf

.PHONY: tree-regen

tree-regen: $(ALL_GEN_FILES)
	${MAKE} -C $(top-build-dir) link-mf-tree

$(outdir)/fonts.conf:
	$(call ly_progress,Making,$@,)
	echo '<fontconfig><dir>'$(shell cd $(outdir); pwd)'</dir></fontconfig>' > $@
