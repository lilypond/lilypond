# mf/Makefile

depth = ..

STEPMAKE_TEMPLATES=metafont metapost asciifont install install-out
include $(depth)/make/stepmake.make 

AF_FILES = $(wildcard *.af) 

EXTRA_DIST_FILES += README feta.tex

# don't try to make fonts from test files
TEST_FILES = $(wildcard *test*.mf)
FET_FILES = $(filter-out $(TEST_FILES),$(wildcard feta[0-9]*.mf))
FONT_FILES = $(filter-out $(TEST_FILES),$(wildcard feta*[0-9].mf))

XPM_FONTS = feta20 feta-nummer10 feta-braces20
#CM_AFM_FILES = cmr10

$(outdir)/cmr10.afm:
	-$(GUILE) $(buildscript-dir)/tfm2oafm.scm `kpsewhich cmr10.tfm`
	-mv $(@F) $@

LOG_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.log))
TEXTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.tex))
AFM_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.afm) $(AF_FILES:.af=.afm) $(addsuffix .afm, $(CM_AFM_FILES)))
TFM_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.tfm))
PFA_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.pfa)) 

# Make tfm files first, log files last, 
# so that normally log files aren't made twice
ALL_GEN_FILES= $(TFM_FILES) $(TEXTABLES) $(AFM_FILES) $(TFM_FILES) $(PFA_FILES) $(LOG_FILES)

#PRE_INSTALL=$(MAKE) "$(ALL_GEN_FILES)"
INSTALLATION_DIR=$(datadir)/mf/
INSTALLATION_FILES=$(MF_FILES) $(AF_FILES)

INSTALLATION_OUT_SUFFIXES=1 2 3 4

INSTALLATION_OUT_DIR1=$(datadir)/tex
INSTALLATION_OUT_FILES1=$(TEXTABLES)

INSTALLATION_OUT_DIR2=$(datadir)/afm
INSTALLATION_OUT_FILES2=$(AFM_FILES)

INSTALLATION_OUT_DIR3=$(datadir)/tfm
INSTALLATION_OUT_FILES3=$(TFM_FILES)

# comment these out if you don't want pfa's to be generated
# or you don't have metapost.
INSTALLATION_OUT_DIR4=$(datadir)/pfa
INSTALLATION_OUT_FILES4=$(PFA_FILES)

export MFINPUTS:=.:$(MFINPUTS)

default: $(ALL_GEN_FILES)

$(outdir)/%.afm  $(outdir)/%.tex $(outdir)/%.dep: $(outdir)/%.log
	$(PYTHON) $(buildscript-dir)/mf-to-table.py --package=$(topdir) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep)  --afm $(outdir)/$(<F:.log=.afm) --tex $(outdir)/$(<F:.log=.tex) $<

localclean:
	rm -f mfplain.mem mfplain.log mpout.log
	rm -f *.tfm *.log

