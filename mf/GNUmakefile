# mf/Makefile

depth = ..

STEPMAKE_TEMPLATES=metafont metapost

EXTRA_DIST_FILES += TODO README feta.mp mfmp.ini


FET_FILES = $(wildcard feta[0-9]*.mf)

LYTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.ly))
LOG_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.log))
TEXTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.tex))
AFM_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.afm))
XPM_FONTS = feta20

ALL_GEN_FILES=$(LYTABLES) $(TEXTABLES) $(LOG_FILES) $(AFM_FILES)

OUT_DIST_FILES += $(AFM_FILES)
export MFINPUTS:=.:$(MFINPUTS)

include $(depth)/make/stepmake.make 

default: $(ALL_GEN_FILES)


$(outdir)/%.afm $(outdir)/%.ly $(outdir)/%.tex $(outdir)/%.dep: $(outdir)/%.log
	$(PYTHON) $(buildscripts)/mf-to-table.py --package=$(topdir) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep) --ly $(outdir)/$(<F:.log=.ly) --afm $(outdir)/$(<F:.log=.afm) --tex $(outdir)/$(<F:.log=.tex) $<



TEXINSTALL=$(MAKE) INSTALLATION_OUT_DIR=$(datadir)/tex/ depth=.. INSTALLATION_OUT_FILES="$(TEXTABLES)" -f $(stepdir)/install-outfiles.sub.make $@

AFMINSTALL=$(MAKE) depth=.. INSTALLATION_OUT_DIR=$(datadir)/afm/ INSTALLATION_OUT_FILES="$(AFM_FILES)" -f $(stepdir)/install-outfiles.sub.make $@

LYINSTALL=$(MAKE) depth=.. INSTALLATION_OUT_DIR=$(datadir)/ly/ INSTALLATION_OUT_FILES="$(LYTABLES)" -f $(stepdir)/install-outfiles.sub.make $@

localinstall: $(ALL_GEN_FILES)
	$(TEXINSTALL)
	$(LYINSTALL)
	$(AFMINSTALL)

localuninstall:
	$(TEXINSTALL)
	$(LYINSTALL)
	$(AFMINSTALL)

INSTALLATION_DIR=$(datadir)/mf/
INSTALLATION_FILES=$(MF_FILES)

include $(stepdir)/installfiles.make

