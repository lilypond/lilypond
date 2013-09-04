# mf/GNUmakefile

depth = ..

STEPMAKE_TEMPLATES = metafont \
		     install \
		     install-out
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


EXTRA_DIST_FILES += README mf2pt1.mp

STAFF_SIZES = 11 13 14 16 18 20 23 26
BRACES = a b c d e f g h i

OTF_FILES = $(STAFF_SIZES:%=$(outdir)/emmentaler-%.otf) \
	    $(outdir)/emmentaler-brace.otf
PE_SCRIPTS = $(STAFF_SIZES:%=$(outdir)/emmentaler-%.pe)
OTF_TABLES = $(STAFF_SIZES:%=$(outdir)/feta%.otf-table) \
	     $(BRACES:%=$(outdir)/feta-braces-%.otf-table)
SVG_FILES = $(OTF_FILES:%.otf=%.svg)
WOFF_FILES = $(OTF_FILES:%.otf=%.woff)

NCSB_OTFS = $(addprefix $(outdir)/,CenturySchL-Ital.otf \
				   CenturySchL-BoldItal.otf \
				   CenturySchL-Roma.otf \
				   CenturySchL-Bold.otf)

LOG_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.log)
LISP_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.lisp)
ENC_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.enc)
TFM_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.tfm)

$(outdir)/emmentaler-brace.otf-table: $(BRACES:%=$(outdir)/feta-braces-%.otf-table)
	cat $^ > $@

$(outdir)/emmentaler-brace.otf-gtable: $(BRACES:%=$(outdir)/feta-braces-%.otf-gtable)
	echo '(design_size . 20)' > $@



## ugh -- we want this to prevent failing -j2 compiles.
define FETAxx-PREREQUISITES


$(outdir)/feta$(i).otf-table: $(outdir)/feta$(i).lisp \
			    $(outdir)/feta-noteheads$(i).lisp \
			    $(outdir)/feta-flags$(i).lisp \
			    $(outdir)/parmesan$(i).lisp \
			    $(outdir)/parmesan-noteheads$(i).lisp \
			    $(outdir)/feta-alphabet$(i).lisp
endef

$(eval $(foreach i,$(STAFF_SIZES),$(FETAxx-PREREQUISITES)))

$(outdir)/%.otf-table: $(outdir)/%.lisp
	cat $< $(if $(findstring brace,$<),,$(subst feta,parmesan,$<)) \
	       $(if $(findstring brace,$<),,$(subst feta,parmesan-noteheads,$<)) \
	       $(if $(findstring brace,$<),,$(subst feta,feta-noteheads,$<)) \
	       $(if $(findstring brace,$<),,$(subst feta,feta-flags,$<)) \
	       $(if $(findstring brace,$<),,$(subst feta,feta-alphabet,$<)) > $@


$(outdir)/emmentaler-brace.otf: $(outdir)/emmentaler-brace.subfonts \
				$(outdir)/emmentaler-brace.fontname \
				$(outdir)/emmentaler-brace.otf-table \
				$(outdir)/emmentaler-brace.otf-gtable \
				$(outdir)/emmentaler-brace.pe

$(outdir)/emmentaler-brace.otf\
 $(outdir)/emmentaler-brace.svg\
 $(outdir)/emmentaler-brace.woff: $(BRACES:%=$(outdir)/feta-braces-%.pfb)

$(outdir)/emmentaler-brace.fontname:
	printf 'emmentaler-brace' > $@
$(outdir)/emmentaler-brace.subfonts:
	echo $(subst .mf,,$(call src-wildcard,feta-braces-[a-z].mf)) > $@

$(PE_SCRIPTS): $(buildscript-dir)/gen-emmentaler-scripts
	$< --dir=$(outdir)


# Generate emmentaler-*.pe scripts first, and *.otf, *.svg, *.woff files last,
# so that normally these files aren't regenerated on a subsequent call to make.
ALL_GEN_FILES = $(PE_SCRIPTS) \
		$(LOG_FILES) \
		$(ENC_FILES) \
		$(LISP_FILES) \
		$(OTF_TABLES) \
		$(NCSB_OTFS) \
		$(OTF_FILES) \
		$(SVG_FILES) \
		$(WOFF_FILES)

# PRE_INSTALL=$(MAKE) "$(ALL_GEN_FILES)"

INSTALLATION_DIR = $(local_lilypond_datadir)/fonts/source
INSTALLATION_FILES = $(MF_FILES)

INSTALLATION_OUT_SUFFIXES = 1 2

INSTALLATION_OUT_DIR1 = $(local_lilypond_datadir)/fonts/otf
INSTALLATION_OUT_FILES1 = $(OTF_FILES) \
			  $(NCSB_OTFS)

INSTALLATION_OUT_DIR2 = $(local_lilypond_datadir)/fonts/svg
INSTALLATION_OUT_FILES2 = $(SVG_FILES) $(WOFF_FILES)

export MFINPUTS := .:$(MFINPUTS)

# only for fonts which
#
# 1. are mentioned in font.scm
#
# 2. are not included with teTeX
#
$(outdir)/%.lisp \
$(outdir)/%.otf-gtable \
$(outdir)/%.enc \
$(outdir)/%.pe: $(outdir)/%.log $(outdir)/%.tfm
	$(buildscript-dir)/mf-to-table \
		--global-lisp=$(outdir)/$(<F:.log=.otf-gtable) \
		--lisp=$(outdir)/$(<F:.log=.lisp) \
		--outdir=$(outdir) \
		--enc $(outdir)/$(<F:.log=.enc) \
		$<

## Putting pfb here forces all .pfb fonts to be built before
# fontforge starts generating emmentaler-*.* fonts.
$(outdir)/emmentaler-%.otf \
$(outdir)/emmentaler-%.svg \
$(outdir)/emmentaler-%.woff: $(outdir)/emmentaler-%.pe \
			    $(outdir)/feta%.pfb \
			    $(outdir)/feta-noteheads%.pfb \
			    $(outdir)/feta-flags%.pfb \
			    $(outdir)/feta-alphabet%.pfb \
			    $(outdir)/parmesan%.pfb \
			    $(outdir)/parmesan-noteheads%.pfb \
			    $(outdir)/feta%.otf-table \
			    $(outdir)/feta%.otf-gtable \
			    | pfb
	cd $(outdir) && $(FONTFORGE) -script $(notdir $<)

$(outdir)/emmentaler-brace.otf\
 $(outdir)/emmentaler-brace.svg\
 $(outdir)/emmentaler-brace.woff: $(outdir)/emmentaler-brace.pe\
		       $(foreach s,$(BRACES),$(outdir)/feta-braces-$(s).pfb) \
		       $(outdir)/emmentaler-brace.otf-table $(outdir)/emmentaler-brace.otf-gtable \
		       | pfb
	cd $(outdir) && $(FONTFORGE) -script emmentaler-brace.pe

default: $(ALL_GEN_FILES) \
	 $(outdir)/emmentaler-20.otf \
	 tree-regen \
	 $(outdir)/fonts.conf

.PHONY: tree-regen

# FIXME: temporary hack: must regenerate after building fonts
tree-regen: $(ALL_GEN_FILES)
	${MAKE} -C $(top-build-dir) link-mf-tree

$(outdir)/%.lisp \
$(outdir)/%.otf-gtable \
$(outdir)/%.enc \
$(outdir)/%.pe: $(outdir)/%.log $(outdir)/%.tfm
	$(buildscript-dir)/mf-to-table \
		--global-lisp=$(outdir)/$(<F:.log=.otf-gtable) \
		--lisp=$(outdir)/$(<F:.log=.lisp) \
		--outdir=$(outdir) \
		--enc $(outdir)/$(<F:.log=.enc) \
		$<

local-clean:
	rm -f mfplain.mem mfplain.log
	rm -f *.tfm *.log


$(outdir)/fonts.conf:
	echo '<fontconfig><dir>'$(shell cd $(outdir); pwd)'</dir></fontconfig>' > $@

$(NCSB_OTFS): $(NCSB_SOURCE_FILES) \
	      $(auxscript-dir)/pfx2ttf.fontforge
	$(foreach i, $(basename $(NCSB_SOURCE_FILES)), \
		$(FONTFORGE) -script $(auxscript-dir)/pfx2ttf.fontforge \
			$(i).pfb $(i).afm $(outdir)/ && ) true
# eof
