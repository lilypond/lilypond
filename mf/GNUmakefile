depth = ..

STEPMAKE_TEMPLATES=metafont install install-out
LOCALSTEPMAKE_TEMPLATES=asciifont


include $(depth)/make/stepmake.make 

AF_FILES = $(wildcard *.af) 

EXTRA_DIST_FILES += README feta.tex

# don't try to make fonts from test files
TEST_FILES = $(wildcard *test*.mf)

# What are these?
XXXFET_FILES = $(filter-out $(TEST_FILES),\
	$(wildcard feta[0-9]*.mf)\
	$(wildcard feta-braces*[0-9].mf)\
	$(wildcard feta-din*[0-9].mf)\
	$(wildcard parmesan[0-9]*.mf))

# No braces?
FONT_FILES = $(filter-out $(TEST_FILES),\
	$(wildcard feta[0-9]*.mf)\
	$(wildcard feta-braces*[0-9].mf)\
	$(wildcard feta-din*[0-9].mf)\
	$(wildcard feta-nummer*[0-9].mf)\
	$(wildcard parmesan[0-9]*.mf))

# let's see what happens
FET_FILES=$(FONT_FILES)

XPM_FONTS = feta20 feta-nummer10 feta-braces20
#CM_AFM_FILES = cmr10

$(outdir)/cmr10.afm:
	-$(GUILE) $(buildscript-dir)/tfm2oafm.scm `kpsewhich cmr10.tfm`
	-mv $(@F) $@

LOG_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.log) $(PARMESAN_FILES:.mf=.log))
TEXTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.tex) $(PARMESAN_FILES:.mf=.tex))
AFM_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.afm) $(PARMESAN_FILES:.mf=.afm) $(AF_FILES:.af=.afm) $(addsuffix .afm, $(CM_AFM_FILES)))
ENC_FILES=$(TEXTABLES:.tex=.enc)
TFM_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.tfm))


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

# comment this out if you don't want pfa's to be generated
# making pfas takes a lot of CPU time. Let's skip it for now.
#MAKE_PFA_FILES=1
#MAKE_PFB_FILES=1


SAUTER_FONT_FILES=cmbxti7 cmbxti8 cmbxti12  cmbxti14 cmcsc7 cmcsc12 cmtt17 cmbx14 cmbx17

PFA_FILES=$(SAUTER_FONT_FILES:%=$(outdir)/%.pfa)
PFA_FILES+=$(FONT_FILES:%.mf=$(outdir)/%.pfa)

PFB_FILES=$(PFA_FILES:%.pfa=%.pfb)

ifdef MAKE_PFB_FILES
MAKE_PFA_FILES = 1
ALL_GEN_FILES += $(PFB_FILES)
endif

ifdef MAKE_PFA_FILES
ALL_GEN_FILES += $(PFA_FILES) $(outdir)/lilypond.map $(outdir)/fonts.scale
INSTALLATION_OUT_DIR4=$(local_lilypond_datadir)/fonts/type1
INSTALLATION_OUT_FILES4=$(PFA_FILES) $(outdir)/fonts.scale

INSTALLATION_OUT_DIR5=$(local_lilypond_datadir)/dvips/
INSTALLATION_OUT_FILES5=$(outdir)/lilypond.map
endif

ifdef MAKE_PFB_FILES
INSTALLATION_OUT_FILES4 +=$(PFB_FILES)
endif

export MFINPUTS:=.:$(MFINPUTS)

default: $(ALL_GEN_FILES)

##
## todo: this also depends on .tfm, FIXME.
$(outdir)/%.afm  $(outdir)/%.enc  $(outdir)/%.tex $(outdir)/%.dep: $(outdir)/%.log $(outdir)/%.tfm
	$(PYTHON) $(buildscript-dir)/mf-to-table.py --package=$(topdir) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep)  --afm $(outdir)/$(<F:.log=.afm)  --enc $(outdir)/$(<F:.log=.enc) --tex $(outdir)/$(<F:.log=.tex) --ly $(outdir)/$(<F:.log=list.ly) $<

fontdir: $(addprefix $(outdir)/, lilypond.map lilypond.sfd fonts.scale fonts.dir)

$(outdir)/lilypond.map: 
	echo $(notdir $(PFA_FILES:.pfa=)) | tr ' ' '\n' | \
		sed 's/\(.*\)/\1 \1 <\1.pfa/' > $@

$(outdir)/fonts.scale: $(PFA_FILES)
#	cd $(outdir) && echo $(FONT_FILES:.mf=.pfa) $(FONT_FILES:.mf=.pfb) | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)
	cd $(outdir) && echo *.pfa *.pfb | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)

$(outdir)/fonts.dir: $(outdir)/fonts.scale
	cd $(outdir) && mkfontdir

# Sketch map file
$(outdir)/lilypond.sfd:
#	cd $(outdir) && echo sketch $(FONT_FILES:.mf=.pfa) | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)
	cd $(outdir) && echo sketch *.pfa | $(PYTHON) $(topdir)/buildscripts/make-font-dir.py  > $(@F)


local-clean:
	rm -f mfplain.mem mfplain.log 
	rm -f *.tfm *.log

afm: $(AFM_FILES)
pfa: afm $(PFA_FILES) fontdir
pfb: afm $(PFB_FILES) fontdir

# needed for web documentation
$(SAUTER_FONT_FILES:%=$(outdir)/%.pfa):
	$(foreach i, $(SAUTER_FONT_FILES), \
		$(MFTRACE) -I $(outdir)/ --pfa --simplify --keep-trying $(i) && mv $(i).pfa $(outdir)/ && ) true
$(SAUTER_FONT_FILES:%=$(outdir)/%.pfb):
	$(foreach i, $(SAUTER_FONT_FILES), \
		$(MFTRACE) -I $(outdir)/ --pfa --pfb --simplify --keep-trying $(i) && mv $(i).pfb $(i).pfa $(outdir)/ && ) true
