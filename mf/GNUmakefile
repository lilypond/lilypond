# mf/Makefile

depth = ..

STEPMAKE_TEMPLATES=metafont metapost install install-out

EXTRA_DIST_FILES += TODO README feta.mp mfplain.ini

FET_FILES = $(wildcard feta[0-9]*.mf)

LOG_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.log))
TEXTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.tex))
XPM_FONTS = feta20
AFM_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.afm)) 
TFM_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.tfm)) 

ALL_GEN_FILES= $(TEXTABLES) $(LOG_FILES) $(AFM_FILES) $(TFM_FILES)

#PRE_INSTALL=$(MAKE) "$(ALL_GEN_FILES)"
INSTALLATION_DIR=$(datadir)/mf/
INSTALLATION_FILES=$(MF_FILES)

INSTALLATION_OUT_SUFFIXES=1 2 3

INSTALLATION_OUT_DIR1=$(datadir)/tex
INSTALLATION_OUT_FILES1=$(TEXTABLES)

INSTALLATION_OUT_DIR2=$(datadir)/afm
INSTALLATION_OUT_FILES2=$(AFM_FILES)

INSTALLATION_OUT_DIR3=$(datadir)/tfm
INSTALLATION_OUT_FILES3=$(TFM_FILES)

export MFINPUTS:=.:$(MFINPUTS)
include $(depth)/make/stepmake.make 

default: $(ALL_GEN_FILES)

$(outdir)/%.afm  $(outdir)/%.tex $(outdir)/%.dep: $(outdir)/%.log
	$(PYTHON) $(buildscripts)/mf-to-table.py --package=$(topdir) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep)  --afm $(outdir)/$(<F:.log=.afm) --tex $(outdir)/$(<F:.log=.tex) $<

localclean:
	rm -f mfplain.mem mfplain.log
