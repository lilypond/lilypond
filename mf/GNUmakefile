# mf/Makefile

depth = ..

STEPMAKE_TEMPLATES=metafont metapost install install-out

EXTRA_DIST_FILES += TODO README feta.mp mfplain.ini

FET_FILES = $(wildcard feta[0-9]*.mf) feta-din10.mf

LYTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.ly))
LOG_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.log))
TEXTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.tex))
AFM_IN_FILES = $(wildcard *.afm.in)
AFM_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.afm)) \
  $(addprefix $(outdir)/, $(AFM_IN_FILES:.in=))
XPM_FONTS = feta20

ALL_GEN_FILES=$(LYTABLES) $(TEXTABLES) $(LOG_FILES) $(AFM_FILES)

#PRE_INSTALL=$(MAKE) "$(ALL_GEN_FILES)"
INSTALLATION_DIR=$(datadir)/mf/
INSTALLATION_FILES=$(MF_FILES)

INSTALLATION_OUT_DIR=$(datadir)/tex
INSTALLATION_OUT_FILES=$(TEXTABLES)

INSTALLATION_OUT_DIR1=$(datadir)/afm
INSTALLATION_OUT_FILES1=$(AFM_FILES)

INSTALLATION_OUT_DIR2=$(datadir)/ly
INSTALLATION_OUT_FILES2=$(LYTABLES)
export MFINPUTS:=.:$(MFINPUTS)
include $(depth)/make/stepmake.make 

default: $(ALL_GEN_FILES)

$(outdir)/%.afm $(outdir)/%.ly $(outdir)/%.tex $(outdir)/%.dep: $(outdir)/%.log
	$(PYTHON) $(buildscripts)/mf-to-table.py --package=$(topdir) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep) --ly $(outdir)/$(<F:.log=.ly) --afm $(outdir)/$(<F:.log=.afm) --tex $(outdir)/$(<F:.log=.tex) $<

