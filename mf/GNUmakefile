depth = ..

STEPMAKE_TEMPLATES=metafont install install-out

include $(depth)/make/stepmake.make 

AF_FILES = $(wildcard *.af) 

EXTRA_DIST_FILES += README feta.tex

# We don't use $(MF_FILES), because there's more .mf cruft here
FETA_MF_FILES = $(wildcard feta[0-9]*.mf)\
	$(wildcard feta-braces-[a-z].mf)\
	$(wildcard feta-alphabet*[0-9].mf)\
	$(wildcard parmesan[0-9]*.mf)

STAFF_SIZES = 11 13 14 16 18 20 23 26
BRACES = a b c d e f g h i

OTF_FILES = $(addsuffix .otf, $(addprefix $(outdir)/bigcheese, $(STAFF_SIZES)))\
 $(outdir)/aybabtu.otf
PE_SCRIPTS = $(addsuffix .pe, $(addprefix $(outdir)/bigcheese, $(STAFF_SIZES))) \
  $(addsuffix .dep, $(addprefix $(outdir)/bigcheese, $(STAFF_SIZES))) 
OTF_TABLES = $(addsuffix .otf-table, $(addprefix $(outdir)/feta, $(STAFF_SIZES))) \
 $(BRACES:%=$(outdir)/feta-braces-%.otf-table)
FETA_FONTS = $(FETA_MF_FILES:.mf=)
SVG_FILES = $(OTF_FILES:%.otf=%.svg)

$(outdir)/aybabtu.otf-table: $(BRACES:%=$(outdir)/feta-braces-%.otf-table)
	cat $^ > $@

$(outdir)/aybabtu.otf-gtable: $(BRACES:%=$(outdir)/feta-braces-%.otf-gtable)
	echo '(design_size . 20)' > $@

LOG_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.log)
LISP_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.lisp)
TEXTABLES = $(FETA_MF_FILES:%.mf=$(outdir)/%.tex)
ENC_FILES = $(TEXTABLES:.tex=.enc) $(outdir)/cmr.enc
TFM_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%.tfm) $(addprefix $(outdir)/,$(addsuffix .tfm,$(SAUTER_FONTS)))
FETA_LIST_FILES = $(FETA_MF_FILES:%.mf=$(outdir)/%list.ly)


ENCODING_FILE=$(findstring $(<:.mf=.enc), $(FETA_MF_FILES:.mf=.enc))
MFTRACE_FLAGS=$(if $(ENCODING_FILE),--encoding $(ENCODING_FILE),)

# only for fonts which
#
# 1. are mentioned in font.scm
#
# 2. are not included with teTeX
#

foe:
	echo $(MFINPUTS)
	echo $(TEXMF)
	kpsewhich exbase.mf

$(outdir)/%.otf $(outdir)/%.svg: $(outdir)/%.pe
	(cd $(outdir) && fontforge -script $(notdir $<)) 

$(outdir)/%.otf-table: $(outdir)/%.lisp
	cat $< $(wildcard $(subst feta,parmesan,$<)) $(wildcard $(subst feta,feta-alphabet,$<)) > $@

$(outdir)/aybabtu.otf: $(outdir)/aybabtu.subfonts $(outdir)/aybabtu.otf-table $(outdir)/aybabtu.otf-gtable

$(outdir)/aybabtu.subfonts: 
	 echo $(subst .mf,,$(wildcard feta-braces-[a-z].mf)) > $@

$(PE_SCRIPTS):
	$(PYTHON) $(buildscript-dir)/gen-bigcheese-scripts.py --dir=$(outdir)

include $(outdir)/bigcheese20.dep


ALL_FONTS = $(FETA_FONTS) $(SAUTER_FONTS)
PFA_FILES = $(ALL_FONTS:%=$(outdir)/%.pfa)
PFB_FILES = $(PFA_FILES:%.pfa=%.pfb)

# Make tfm files first, log files last, 
# so that normally log files aren't made twice
ALL_GEN_FILES= $(TFM_FILES) $(TEXTABLES) $(TFM_FILES) $(LOG_FILES) $(ENC_FILES) $(LISP_FILES) $(FETA_LIST_FILES)  $(OTF_TABLES) $(PFA_FILES) $(outdir)/lilypond.map $(outdir)/fonts.scale $(outdir)/Fontmap.lily $(OTF_FILES)

#PRE_INSTALL=$(MAKE) "$(ALL_GEN_FILES)"
INSTALLATION_DIR=$(local_lilypond_datadir)/fonts/source
INSTALLATION_FILES=$(MF_FILES) $(AF_FILES)

INSTALLATION_OUT_SUFFIXES=1 2 3 4 5 6 7

INSTALLATION_OUT_DIR1=$(local_lilypond_datadir)/tex
INSTALLATION_OUT_FILES1=$(TEXTABLES)

INSTALLATION_OUT_DIR2=$(local_lilypond_datadir)/otf
INSTALLATION_OUT_FILES2=$(OTF_FILES)

INSTALLATION_OUT_DIR3=$(local_lilypond_datadir)/fonts/tfm
INSTALLATION_OUT_FILES3=$(TFM_FILES)

INSTALLATION_OUT_DIR4=$(local_lilypond_datadir)/ly
INSTALLATION_OUT_FILES4=$(FETA_LIST_FILES)

INSTALLATION_OUT_DIR5=$(local_lilypond_datadir)/fonts/type1
INSTALLATION_OUT_FILES5=$(PFA_FILES) $(outdir)/fonts.scale $(outdir)/Fontmap $(outdir)/Fontmap.lily

INSTALLATION_OUT_DIR6=$(local_lilypond_datadir)/dvips/
INSTALLATION_OUT_FILES6=$(outdir)/lilypond.map

INSTALLATION_OUT_DIR7=$(local_lilypond_datadir)/ps/
INSTALLATION_OUT_FILES7=$(ENC_FILES)

INSTALLATION_OUT_DIR8=$(local_lilypond_datadir)/svg
INSTALLATION_OUT_FILES8=$(OTF_FILES)


export MFINPUTS:=.:$(MFINPUTS)

default: pfa_warning $(ALL_GEN_FILES) $(outdir)/bigcheese20.otf


pfa_warning:
ifneq ($(notdir $(MFTRACE)),mftrace)
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
	cp `find $(outdir)/usr/ -name '*.pfa'` $(outdir)/


redhat-package=lilypond-$(TOPLEVEL_VERSION)-1.i386.rpm
$(outdir)/$(redhat-package):
	wget  --passive-ftp -P $(outdir) http://lilypond.org/download/binaries/Fedora-3/$(redhat-package)

get-rpm-pfa: $(outdir)/$(redhat-package)
	cd $(outdir) ; rm -rf usr/ # (root alert!) 
	cd $(outdir) ; rpm2cpio $(redhat-package) | cpio -uid '*.pfa'
	cp `find $(outdir)/usr/ -name '*.pfa'` $(outdir)/

# change this dependency 
get-pfa: get-rpm-pfa


##
## todo: this also depends on .tfm, FIXME.
$(outdir)/%.lisp $(outdir)/%.otf-gtable $(outdir)/%.enc  $(outdir)/%.tex $(outdir)/%list.ly $(outdir)/%.dep: $(outdir)/%.log $(outdir)/%.tfm
	$(PYTHON) $(buildscript-dir)/mf-to-table.py --global-lisp=$(outdir)/$(<F:.log=.otf-gtable) --lisp=$(outdir)/$(<F:.log=.lisp) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep) --enc $(outdir)/$(<F:.log=.enc) --tex $(outdir)/$(<F:.log=.tex) --ly $(outdir)/$(<F:.log=list.ly) $<

fontdir: $(addprefix $(outdir)/, lilypond.map lilypond.sfd private-fonts fonts.scale fonts.dir Fontmap.lily)

## Urg
mapentry=$(1) $(shell fgrep FontName $(outdir)/$(1).afm | sed -e 's/FontName *//') <$(1).pfaX

map = $(foreach a,$(2),$(call $(1),$(a)))
$(outdir)/lilypond.xmap: $(AFM_FILES)
	echo '$(call map,mapentry,$(ALL_FONTS))' | \
		tr 'X' '\n' | sed -e 's/^ *//'> $@

# using shell for loop seems most robust
$(outdir)/lilypond.map: $(AFM_FILES)
	for i in $(FETA_FONTS); do echo $$i $$(fgrep FontName $(outdir)/$$i.afm | sed -e 's/FontName *//') '<'$$i.pfa; done > $@

$(outdir)/Fontmap:
	echo '%!' > $@
	echo '% Override default GS Fontmap' >> $@
	echo '% To let gs load fonts from builddir, do:' >> $@
	echo '% export GS_LIB=$$(pwd)/mf/out:' >> $@
	echo '% See Fontmap.GS for the syntax of real Fontmap files.' >> $@
	echo '(Fontmap.GS) .runlibfile' >> $@
	echo '(Fontmap.lily) .runlibfile' >> $@

$(outdir)/Fontmap.lily: $(AFM_FILES) $(outdir)/Fontmap
	echo '%!' > $@
	for i in $(FETA_FONTS); do echo "/$$(fgrep FontName $(outdir)/$$i.afm | sed -e 's/FontName *//') ($$i.pfa);"; done >> $@


$(outdir)/fonts.scale: $(PFA_FILES)
#	cd $(outdir) && echo $(FETA_MF_FILES:.mf=.pfa) $(FETA_MF_FILES:.mf=.pfb) | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)
	cd $(outdir) && echo *.pfa *.pfb | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)

$(outdir)/fonts.dir: $(outdir)/fonts.scale
	cd $(outdir) && mkfontdir

local-clean:
	rm -f mfplain.mem mfplain.log 
	rm -f *.tfm *.log

afm: $(AFM_FILES)
pfa: afm $(PFA_FILES) fontdir
pfb: afm $(PFB_FILES) fontdir


# stupid trick to have separate rules for each PFA font ;
# otherwise building PFAs is all-or-nothing.


$(outdir)/%.pfa: $(outdir)/%.bla
	$(MFTRACE) -I $(outdir)/ --pfa --simplify --keep-trying $(notdir $(basename $@))  && mv $(notdir $@) $(outdir)/

$(outdir)/%.enc.in: %.enc
	cp $< $@
