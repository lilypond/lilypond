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

.PHONY: tfm dvi pfb

pfb: $(PFB_FILES)

MF2PT1_OPTIONS=--rounding=0.0001 \
               --family=$(notdir $(<:%.mf=%)) \
               --fullname=$(notdir $(<:%.mf=%)) \
               --name=$(notdir $(<:%.mf=%))

# Don't remove $(outdir)/.log's.  Logs are a target!

# we want to see botched results as well.
$(outdir)/%.dvi: %.mf
	$(call ly_progress,Making,$@,< mf)
	-$(DO_MF_DEP) \
	  MFINPUTS=$(src-dir) \
	  max_print_line=1000 \
	  $(METAFONT) "\scrollmode; input $<;" $(METAFONT_QUIET)
	gftodvi $(basename $<)
	mv $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.tfm $(outdir)/%.log: %.mf
	$(call ly_progress,Making,$@,< mf)
	$(DO_MF_DEP) \
	  MFINPUTS=$(src-dir) \
	  max_print_line=1000 \
	  $(METAFONT) "\mode:=$(MFMODE); nonstopmode; input $<;" $(METAFONT_QUIET)
# Let's keep this log output, it saves another mf run.
	mv $(basename $(@F)).log $(basename $(@F)).tfm $(outdir)
	rm -f $(basename $(@F)).*gf  $(basename $(@F)).*pk

# ugh . mf2pt1 is extremely broken, it pollutes CWD iso. creating a
# temp dir.
#
# the soft link for mf2pt1.mp is for recent mpost versions
# which no longer dump a .mem file
$(outdir)/%.pfb: %.mf $(outdir)/mf2pt1.mem $(outdir)/%.log
	$(call ly_progress,Making,$@,< mf)
	$(DO_MF_DEP) TMP_DIR=`mktemp -d $(outdir)/pfbtemp.$*.XXXXXXXXX` \
	&& ( cd $$TMP_DIR \
		&& ln -s ../mf2pt1.mem . \
		&& ln -s ../../mf2pt1.mp . \
		&& MFINPUTS=$(abs-src-dir):..:: \
		   FONTFORGE=$(FONTFORGE) \
		   max_print_line=1000 \
		   $(buildscript-dir)/mf2pt1 $(MF2PT1_OPTIONS) $< $(METAFONT_QUIET)) \
	&& mv $$TMP_DIR/*pfb $(outdir); \
	rm -rf $$TMP_DIR

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

# Find the metafont file $(1) within the source dirs and return its path.
# If not found, return $(outdir)/$(1) assuming that it is a generated file.
find-mf = \
$(firstword \
	$(wildcard $(src-dir)/$(1)) \
	$(wildcard $(top-src-dir)/mf/$(1)) \
	$(outdir)/$(1) \
)

# Recursively scan the metafont .mf file $(1) for "input X;"
# and return all dependencies.
scan-mf = \
$(foreach f, $(shell test -f $(1) && sed -ne "/^[[:space:]]*input[[:space:]]/s/^[[:space:]]*input\([^.;]*\)\(.mf;\|;\)/\1.mf/p" $(1)), \
	$(call find-mf,$(f)) \
	$(call scan-mf,$(call find-mf,$(f))) \
)

# Find dependencies for the target $@, based on the metafont source file $<,
# and write the dependencies to a .dep file. We cannot strip the extension of $@,
# because we have multiple rules generating .dep files.
DO_MF_DEP = ( echo ./$@: $(call scan-mf,$<) > $@.dep ) &&


EXTRA_DIST_FILES += README mf2pt1.mp

STAFF_SIZES = 11 13 14 16 18 20 23 26
BRACES = a b c d e f g h i

OTF_FILES = $(STAFF_SIZES:%=$(outdir)/emmentaler-%.otf) \
	    $(outdir)/emmentaler-brace.otf
OTF_TABLES = $(STAFF_SIZES:%=$(outdir)/feta%.otf-table) \
	     $(BRACES:%=$(outdir)/feta-braces-%.otf-table)
SVG_FILES = $(OTF_FILES:%.otf=%.svg)
WOFF_FILES = $(OTF_FILES:%.otf=%.woff)

TEXGYRE_OTFS = $(addprefix $(TEXGYRE_DIR)/,$(TEXGYRE_FILES))
URWOTF_OTFS = $(addprefix $(URWOTF_DIR)/,$(URWOTF_FILES))

LILYPOND_FONTS_CONF = $(outdir)/00-lilypond-fonts.conf \
		      $(outdir)/99-lilypond-fonts.conf

LOG_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.log)
LISP_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.lisp)
ENC_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.enc)
TFM_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.tfm)

$(outdir)/emmentaler-brace.otf-gtable: $(BRACES:%=$(outdir)/feta-braces-%.otf-gtable)
	$(call ly_progress,Making,$@,)
	echo '(design_size . 20)' > $@

$(outdir)/feta%.otf-table: $(outdir)/feta%.lisp $(outdir)/parmesan%.lisp \
	$(outdir)/parmesan-noteheads%.lisp \
	$(outdir)/feta-noteheads%.lisp \
	$(outdir)/feta-flags%.lisp \
	$(outdir)/feta-alphabet%.lisp
	$(call ly_progress,Making,$@,< lisp)
	cat $^ > $@

$(outdir)/emmentaler-brace.otf-table: $(foreach x, a b c d e f g h i,$(outdir)/feta-braces-$(x).lisp)
	$(call ly_progress,Making,$@,< lisp)
	cat $^ > $@

$(outdir)/emmentaler-brace.otf: $(outdir)/emmentaler-brace.subfonts \
				$(outdir)/emmentaler-brace.fontname \
				$(outdir)/emmentaler-brace.otf-table \
				$(outdir)/emmentaler-brace.otf-gtable \
				$(outdir)/emmentaler-brace.pe

$(outdir)/emmentaler-brace.fontname:
	$(call ly_progress,Making,$@,)
	printf 'emmentaler-brace' > $@

$(outdir)/emmentaler-brace.subfonts:
	$(call ly_progress,Making,$@,)
	echo $(subst .mf,,$(call src-wildcard,feta-braces-[a-z].mf)) > $@

$(outdir)/emmentaler-%.genpe: $(buildscript-dir)/gen-emmentaler-scripts
	$(call ly_progress,Making,$@,)
	$< --dir=$(outdir) --design-size=$(patsubst emmentaler-%.genpe,%,$(notdir $@))

ALL_GEN_FILES = $(ENC_FILES) \
		$(OTF_FILES) \
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

export MFINPUTS := .:$(MFINPUTS)

# A few rules here generate multiple files from one command line.  For
# treating this case, we only declare one output explicitly, and use a
# dummy rules for the other outputs. The dummy rule uses
# $(UPDATE_TARGET) so the order of writing in the real command does
# not confuse make.
UPDATE_TARGET = if test -f $@; then touch $@ ; fi

# only for fonts which
#
# 1. are mentioned in font.scm
#
# 2. are not included with teTeX
#
$(outdir)/%.lisp: $(outdir)/%.log $(outdir)/%.tfm
	$(call ly_progress,Making,$@,< log)
	$(buildscript-dir)/mf-to-table \
		--global-lisp=$(outdir)/$(<F:.log=.otf-gtable) \
		--lisp=$(outdir)/$(<F:.log=.lisp) \
		--outdir=$(outdir) \
		--enc $(outdir)/$(<F:.log=.enc) \
		$<

$(outdir)/%.otf-gtable $(outdir)/%.enc: $(outdir)/%.lisp
	$(UPDATE_TARGET)

## Putting pfb here forces all .pfb fonts to be built before
# fontforge starts generating emmentaler-*.* fonts.
$(outdir)/emmentaler-%.otf: $(outdir)/emmentaler-%.genpe \
			    $(outdir)/feta%.pfb \
			    $(outdir)/feta-noteheads%.pfb \
			    $(outdir)/feta-flags%.pfb \
			    $(outdir)/feta-alphabet%.pfb \
			    $(outdir)/parmesan%.pfb \
			    $(outdir)/parmesan-noteheads%.pfb \
			    $(outdir)/feta%.otf-table \
			    $(outdir)/feta%.otf-gtable \
			    | pfb
	$(call ly_progress,Making,$@,)
	cd $(outdir) && $(FONTFORGE) -script $(notdir $<)

$(outdir)/emmentaler-%.svg $(outdir)/emmentaler-%.woff: $(outdir)/emmentaler-%.otf
	$(UPDATE_TARGET)

$(outdir)/emmentaler-brace.otf: $(outdir)/emmentaler-brace.pe\
		       $(foreach s,$(BRACES),$(outdir)/feta-braces-$(s).pfb) \
		       $(outdir)/emmentaler-brace.otf-table $(outdir)/emmentaler-brace.otf-gtable \
		       | pfb
	$(call ly_progress,Making,$@,)
	cd $(outdir) && $(FONTFORGE) -script emmentaler-brace.pe

$(outdir)/emmentaler-brace.svg $(outdir)/emmentaler-brace.woff: $(outdir)/emmentaler-brace.otf
	$(UPDATE_TARGET)

default: tree-regen \
	 $(outdir)/fonts.conf

.PHONY: tree-regen

tree-regen: $(ALL_GEN_FILES)
	${MAKE} -C $(top-build-dir) link-mf-tree


local-clean:
	rm -f mfplain.mem mfplain.log
	rm -f *.tfm *.log


$(outdir)/fonts.conf:
	$(call ly_progress,Making,$@,)
	echo '<fontconfig><dir>'$(shell cd $(outdir); pwd)'</dir></fontconfig>' > $@
