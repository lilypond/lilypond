depth = ..

STEPMAKE_TEMPLATES=metafont install install-out
LOCALSTEPMAKE_TEMPLATES=asciifont

include $(depth)/make/stepmake.make 

AF_FILES = $(wildcard *.af) 

EXTRA_DIST_FILES += README feta.tex

# We don't use $(MF_FILES), because there's more .mf cruft here
FETA_MF_FILES = $(wildcard feta[0-9]*.mf)\
	$(wildcard feta-braces*[0-9].mf)\
	$(wildcard feta-din*[0-9].mf)\
	$(wildcard feta-nummer*[0-9].mf)\
	$(wildcard parmesan[0-9]*.mf)

FETA_FONTS = $(FETA_MF_FILES:.mf=)

XPM_FONTS = feta20 feta-din10 feta-nummer10 feta-braces20 parmesan20

LOG_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.log)
TEXTABLES = $(FETA_MF_FILES:%.mf=$(outdir)/%.tex)
AFM_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.afm) \
	$(AF_FILES:%.af=$(outdir)/%.afm)
ENC_FILES = $(TEXTABLES:.tex=.enc)
TFM_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.tfm)


ENCODING_FILE=$(findstring $(<:.mf=.enc), $(FETA_MF_FILES:.mf=.enc))
MFTRACE_FLAGS=$(if $(ENCODING_FILE),--encoding $(ENCODING_FILE),)

# Make tfm files first, log files last, 
# so that normally log files aren't made twice
ALL_GEN_FILES= $(TFM_FILES) $(TEXTABLES) $(AFM_FILES) $(TFM_FILES) $(LOG_FILES) $(ENC_FILES)

#PRE_INSTALL=$(MAKE) "$(ALL_GEN_FILES)"
INSTALLATION_DIR=$(local_lilypond_datadir)/fonts/source
INSTALLATION_FILES=$(MF_FILES) $(AF_FILES)

INSTALLATION_OUT_SUFFIXES=1 2 3 4 5

INSTALLATION_OUT_DIR1=$(local_lilypond_datadir)/tex
INSTALLATION_OUT_FILES1=$(TEXTABLES)

INSTALLATION_OUT_DIR2=$(local_lilypond_datadir)/fonts/afm
INSTALLATION_OUT_FILES2=$(AFM_FILES)

INSTALLATION_OUT_DIR3=$(local_lilypond_datadir)/fonts/tfm
INSTALLATION_OUT_FILES3=$(TFM_FILES)

SAUTER_FONTS = cmbxti7 cmbxti8 cmbxti12 cmbxti14 \
	cmcsc7 cmcsc12 cmtt17 cmbx14 cmbx17 cmtt6 cmbx6 cmbx5 cmbxti5 \
	cmti6 cmti5  cmss5 cmss6 cmss7

ALL_FONTS = $(FETA_FONTS) $(SAUTER_FONTS)


PFA_FILES = $(ALL_FONTS:%=$(outdir)/%.pfa)
PFB_FILES = $(PFA_FILES:%.pfa=%.pfb)

ALL_GEN_FILES += $(PFA_FILES) $(outdir)/lilypond.map $(outdir)/fonts.scale
INSTALLATION_OUT_DIR4=$(local_lilypond_datadir)/fonts/type1
INSTALLATION_OUT_FILES4=$(PFA_FILES) $(outdir)/fonts.scale

INSTALLATION_OUT_DIR5=$(local_lilypond_datadir)/dvips/
INSTALLATION_OUT_FILES5=$(outdir)/lilypond.map

export MFINPUTS:=.:$(MFINPUTS)

default: pfa_warning $(ALL_GEN_FILES)


pfa_warning:
ifneq ($(basename $(MFTRACE)),mftrace)
	@echo ""
	@echo "ERROR: mftrace not found"
	@echo ""
	@echo "For obtaining PFA fonts, either install mftrace "
	@echo "(see http://www.xs4all.nl/~hanwen/mftrace/ ),"
	@echo "or try one of the following commands in this directory: "
	@echo ""
	@echo " make get-pfa "
	@echo " make get-rpm-pfa "
	@echo " make get-deb-pfa "
	@echo ""
	@echo "These commands will try to download the files from the internet."
	@false
endif


debian-mirror=http://ftp.us.debian.org
debian-package =lilypond_$(TOPLEVEL_VERSION)-1_i386.deb
$(outdir)/$(debian-package):
	wget --passive-ftp -P $(outdir) $(debian-mirror)/debian/pool/main/l/lilypond/$(debian-package)

get-deb-pfa: $(outdir)/$(debian-package)
	rm -rf $(outdir)/./usr
	ar p $< data.tar.gz | \
		tar -C $(outdir) -zvxf - '*.pfa'
	touch $(addprefix $(outdir)/, $(addsuffix .bla,$(SAUTER_FONTS)))
	cp `find $(outdir)/usr/ -name '*.pfa'` $(outdir)/


redhat-package=lilypond-$(TOPLEVEL_VERSION)-1.i386.rpm
$(outdir)/$(redhat-package):
	wget  --passive-ftp -P $(outdir) ftp://ftp.lilypond.org/pub/LilyPond/binaries/RedHat-9/$(redhat-package)

get-rpm-pfa: $(outdir)/$(redhat-package)
	cd $(outdir) ; rm -rf usr/ # (root alert!) 
	cd $(outdir) ; rpm2cpio $(redhat-package) | cpio -uid '*.pfa'
	touch $(addprefix $(outdir)/, $(addsuffix .bla,$(SAUTER_FONTS)))
	cp `find $(outdir)/usr/ -name '*.pfa'` $(outdir)/

# change this dependency 
get-pfa: get-rpm-pfa


##
## todo: this also depends on .tfm, FIXME.
$(outdir)/%.afm  $(outdir)/%.enc  $(outdir)/%.tex $(outdir)/%list.ly $(outdir)/%.dep: $(outdir)/%.log $(outdir)/%.tfm
	$(PYTHON) $(buildscript-dir)/mf-to-table.py --package=$(topdir) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep)  --afm $(outdir)/$(<F:.log=.afm)  --enc $(outdir)/$(<F:.log=.enc) --tex $(outdir)/$(<F:.log=.tex) --ly $(outdir)/$(<F:.log=list.ly) $<

fontdir: $(addprefix $(outdir)/, lilypond.map lilypond.sfd private-fonts fonts.scale fonts.dir)

## Urg
mapentry=$(1) $(shell fgrep FontName $(outdir)/$(1).afm | sed -e 's/FontName *//') <$(1).pfaX

map = $(foreach a,$(2),$(call $(1),$(a)))
$(outdir)/lilypond.xmap: $(AFM_FILES)
	echo '$(call map,mapentry,$(ALL_FONTS))' | \
		tr 'X' '\n' | sed -e 's/^ *//'> $@

# using shell for loop seems most robust
$(outdir)/lilypond.map: $(AFM_FILES)
	for i in $(FETA_FONTS); do echo $$i $$(fgrep FontName $(outdir)/$$i.afm | sed -e 's/FontName *//') '<'$$i.pfa; done > $@
	for i in $(SAUTER_FONTS); do echo "$$i $$i <$$i.pfa"; done >> $@


$(outdir)/fonts.scale: $(PFA_FILES)
#	cd $(outdir) && echo $(FETA_MF_FILES:.mf=.pfa) $(FETA_MF_FILES:.mf=.pfb) | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)
	cd $(outdir) && echo *.pfa *.pfb | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)

$(outdir)/fonts.dir: $(outdir)/fonts.scale
	cd $(outdir) && mkfontdir

# Sketch map file
$(outdir)/lilypond.sfd:
#	cd $(outdir) && echo sketch $(FETA_MF_FILES:.mf=.pfa) | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)
	cd $(outdir) && echo sketch *.pfa | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)

# Sodipodi map file
SODIPODI_FONTS = parmesan20 feta-braces0 feta-nummer10 feta20
$(outdir)/private-fonts:
	cd $(outdir) && echo sodipodi $(SODIPODI_FONTS:%=%.pfa) | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)

local-clean:
	rm -f mfplain.mem mfplain.log 
	rm -f *.tfm *.log

afm: $(AFM_FILES)
pfa: afm $(PFA_FILES) fontdir
pfb: afm $(PFB_FILES) fontdir


# stupid trick to have separate rules for each PFA font ;
# otherwise building PFAs is all-or-nothing.
$(SAUTER_FONTS:%=$(outdir)/%.bla):
	$(foreach i, $@, touch $i && ) true

$(outdir)/%.pfa: $(outdir)/%.bla
	$(MFTRACE) -I $(outdir)/ --formats=pfa --simplify --keep-trying $(notdir $(basename $@))  && mv $(notdir $@) $(outdir)/

