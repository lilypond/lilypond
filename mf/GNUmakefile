depth = ..

STEPMAKE_TEMPLATES=metafont install install-out
LOCALSTEPMAKE_TEMPLATES=lilypond

include $(depth)/make/stepmake.make

AF_FILES = $(call src-wildcard,*.af)

# hangs on feta23
MFTRACE_FLAGS=  --simplify

EXTRA_DIST_FILES += README feta.tex

# We don't use $(MF_FILES), because there's more .mf cruft here
FETA_MF_FILES = $(call src-wildcard,feta[0-9]*.mf)\
	$(call src-wildcard,feta-braces-[a-z].mf)\
	$(call src-wildcard,feta-alphabet*[0-9].mf)\
	$(call src-wildcard,parmesan[0-9]*.mf)

STAFF_SIZES = 11 13 14 16 18 20 23 26
BRACES = a b c d e f g h i

OTF_FILES = $(STAFF_SIZES:%=$(outdir)/emmentaler-%.otf)\
 $(outdir)/aybabtu.otf
PE_SCRIPTS = $(STAFF_SIZES:%=$(outdir)/emmentaler-%.pe)\
 $(STAFF_SIZES:%=$(outdir)/emmentaler-%.dep)
OTF_TABLES = $(STAFF_SIZES:%=$(outdir)/feta%.otf-table)\
 $(BRACES:%=$(outdir)/feta-braces-%.otf-table)
FETA_FONTS = $(FETA_MF_FILES:.mf=)
SVG_FILES = $(OTF_FILES:%.otf=%.svg) $(ALL_FONTS:%=$(outdir)/%.svg)


NCSB_SOURCE_FILES=c059013l c059016l c059033l c059036l
NCSB_TTFS=$(addprefix $(outdir)/,CenturySchL-Ital.ttf CenturySchL-BoldItal.ttf CenturySchL-Roma.ttf CenturySchL-Bold.ttf)
NCSB_INSTALL_DIR=$(local_lilypond_datadir)/fonts/otf

LOG_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.log)
LISP_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.lisp)
TEXTABLES = $(FETA_MF_FILES:%.mf=$(outdir)/%.tex)
ENC_FILES = $(TEXTABLES:.tex=.enc)
TFM_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.tfm)\
 $(SAUTER_FONTS:%=$(outdir)/%.tfm)


ENCODING_FILE=$(findstring $(<F:.mf=.enc), $(FETA_MF_FILES:.mf=.enc))
MFTRACE_FLAGS=$(if $(ENCODING_FILE),--encoding $(ENCODING_FILE),) --no-afm

$(outdir)/aybabtu.otf-table: $(BRACES:%=$(outdir)/feta-braces-%.otf-table)
	cat $^ > $@

$(outdir)/aybabtu.otf-gtable: $(BRACES:%=$(outdir)/feta-braces-%.otf-gtable)
	echo '(design_size . 20)' > $@


# only for fonts which
#
# 1. are mentioned in font.scm
#
# 2. are not included with teTeX
#


$(outdir)/emmentaler-%.otf $(outdir)/emmentaler-%.svg: $(outdir)/pe-script-canary $(outdir)/emmentaler-%.pe $(outdir)/feta%.pfa  $(outdir)/feta-alphabet%.pfa  $(outdir)/parmesan%.pfa
	cd $(outdir) && $(FONTFORGE) -script $(notdir $(basename ,$@).pe)

$(outdir)/aybabtu.otf $(outdir)/aybabtu.svg: $(outdir)/aybabtu.pe $(foreach s,$(BRACES),$(outdir)/feta-braces-$(s).pfa)
	cd $(outdir) && $(FONTFORGE) -script aybabtu.pe
	-rm -f $(outdir)/*.scale.{pfa,afm}

$(outdir)/%.pfa: $(outdir)/%.log

$(outdir)/%.otf-table: $(outdir)/%.lisp $(if $(findstring brace,$<),,$(subst feta,parmesan,$<))
	cat $< \
		$(if $(findstring brace,$<),,$(subst feta,parmesan,$<)) \
		$(if $(findstring brace,$<),,$(subst feta,feta-alphabet,$<)) > $@

$(outdir)/aybabtu.otf: $(outdir)/aybabtu.subfonts $(outdir)/aybabtu.fontname $(outdir)/aybabtu.otf-table $(outdir)/aybabtu.otf-gtable $(outdir)/aybabtu.pe

$(outdir)/aybabtu.otf $(outdir)/aybabtu.svg: $(BRACES:%=$(outdir)/feta-braces-%.pfa)

$(outdir)/aybabtu.fontname:
	echo -n 'aybabtu' > $@
$(outdir)/aybabtu.subfonts:
	echo $(subst .mf,,$(call src-wildcard,feta-braces-[a-z].mf)) > $@

$(outdir)/pe-script-canary: $(buildscript-dir)/gen-emmentaler-scripts.py
	$(PYTHON) $< --dir=$(outdir)

$(outdir)/fonts.cache-1: $(PFA_FILES) $(OTF_FILES)
	cd $(outdir) ; fc-cache .

local-install: install-fc-cache

install-fc-cache:
	$(foreach suff, $(INSTALLATION_OUT_SUFFIXES), rm -f $(DESTDIR)$(INSTALLATION_OUT_DIR$(suff))/fonts.cache-1 && ) true
	fc-cache $(foreach suff, $(INSTALLATION_OUT_SUFFIXES), $(DESTDIR)$(INSTALLATION_OUT_DIR$(suff)))

ALL_FONTS = $(FETA_FONTS) $(SAUTER_FONTS)
PFA_FILES = $(ALL_FONTS:%=$(outdir)/%.pfa)

# Make tfm files first, log files last,
# so that normally log files aren't made twice
ALL_GEN_FILES= $(TFM_FILES) $(TEXTABLES) $(TFM_FILES) $(LOG_FILES) $(ENC_FILES) $(LISP_FILES) $(FETA_LIST_FILES)  $(OTF_TABLES) $(NCSB_TTFS) $(PFA_FILES) $(OTF_FILES) $(SVG_FILES) $(outdir)/fonts.cache-1

#PRE_INSTALL=$(MAKE) "$(ALL_GEN_FILES)"

INSTALLATION_DIR=$(local_lilypond_datadir)/fonts/source
INSTALLATION_FILES=$(MF_FILES) $(AF_FILES)

INSTALLATION_OUT_SUFFIXES=1 2 3 4 5 6

INSTALLATION_OUT_DIR1=$(local_lilypond_datadir)/tex
INSTALLATION_OUT_FILES1=$(TEXTABLES)

INSTALLATION_OUT_DIR2=$(local_lilypond_datadir)/fonts/otf
INSTALLATION_OUT_FILES2=$(OTF_FILES) $(NCSB_TTFS)

INSTALLATION_OUT_DIR3=$(local_lilypond_datadir)/fonts/tfm
INSTALLATION_OUT_FILES3=$(TFM_FILES)

INSTALLATION_OUT_DIR4=$(local_lilypond_datadir)/fonts/svg
INSTALLATION_OUT_FILES4=$(SVG_FILES)

INSTALLATION_OUT_DIR5=$(local_lilypond_datadir)/fonts/type1
INSTALLATION_OUT_FILES5=$(PFA_FILES)

export MFINPUTS:=.:$(MFINPUTS)



default: pfa_warning $(ALL_GEN_FILES) $(outdir)/emmentaler-20.otf tree-regen

.PHONY: tree-regen

# FIXME: temporary hack: must regenerate after building fonts
tree-regen:
	${MAKE} -C $(top-build-dir) link-mf-tree

pfa_warning:
ifneq ($(shell $(MFTRACE) --version | sed 's/ .*//'),mftrace)
	@echo ""
	@echo "ERROR: mftrace not found"
	@echo ""
	@echo "For obtaining PFA/SVG fonts, either install mftrace "
	@echo "(see http://www.xs4all.nl/~hanwen/mftrace/ ),"
	@echo "or try one of the following commands in this directory: "
	@echo ""
	@echo " make get-fonts "
	@echo " make get-rpm-fonts "
	@echo " make get-deb-fonts "
	@echo ""
	@echo "These commands will try to download the files from internet."
	@false
endif


debian-mirror=http://ftp.us.debian.org
debian-package =lilypond_$(TOPLEVEL_VERSION)-1_i386.deb
$(outdir)/$(debian-package):
	wget --passive-ftp -P $(outdir) $(debian-mirror)/debian/pool/main/l/lilypond/$(debian-package)

get-deb-fonts: $(outdir)/$(debian-package)
	rm -rf $(outdir)/./usr
	ar p $< data.tar.gz | \
		tar -C $(outdir) -zvxf - '*.pfa' '*.svg'
	cp `find $(outdir)/usr/ -name '*.pfa' -o -name '*.svg'` $(outdir)/


redhat-package=lilypond-$(TOPLEVEL_VERSION)-1.i386.rpm
$(outdir)/$(redhat-package):
	wget  --passive-ftp -P $(outdir) http://lilypond.org/download/binaries/Fedora-4/$(redhat-package)

get-rpm-fonts: $(outdir)/$(redhat-package)
	cd $(outdir) ; rm -rf usr/ # (root alert!)
	cd $(outdir) ; rpm2cpio $(redhat-package) | cpio -uid '*.pfa' '*.svg'
	cp `find $(outdir)/usr/ -name '*.pfa' -o -name '*.svg'` $(outdir)/

# change this dependency
get-fonts: get-rpm-fonts
get-pfa: get-rpm-fonts


##
## todo: this also depends on .tfm, FIXME.
$(outdir)/%.lisp $(outdir)/%.otf-gtable $(outdir)/%.enc  $(outdir)/%.tex $(outdir)/%.dep: $(outdir)/%.log $(outdir)/%.tfm
	$(PYTHON) $(buildscript-dir)/mf-to-table.py --global-lisp=$(outdir)/$(<F:.log=.otf-gtable) --lisp=$(outdir)/$(<F:.log=.lisp) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep) --enc $(outdir)/$(<F:.log=.enc) --tex $(outdir)/$(<F:.log=.tex) $<

fontdir: $(addprefix $(outdir)/, lilypond.map lilypond.sfd private-fonts fonts.scale fonts.dir Fontmap.lily)


local-clean:
	rm -f mfplain.mem mfplain.log
	rm -f *.tfm *.log

$(outdir)/%.enc.in: %.enc
	cp $< $@


$(NCSB_TTFS): $(addsuffix .pfb,$(addprefix $(NCSB_DIR)/,$(NCSB_SOURCE_FILES))) $(buildscript-dir)/pfx2ttf.fontforge
	$(foreach i,$(NCSB_SOURCE_FILES), \
		$(FONTFORGE) -script $(buildscript-dir)/pfx2ttf.fontforge \
			$(NCSB_DIR)/$(i).pfb $(NCSB_DIR)/$(i).afm $(outdir)/ && ) true
