# mf/Makefile

depth = ..

MAKEFILES=Metafont

EXTRA_DIST_FILES += TODO README

FET_FILES = $(wildcard feta[0-9]*.mf)

LYTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.ly))
LOG_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.log))
TEXTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.tex))
AFM_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.afm))

ALL_GEN_FILES=$(LYTABLES) $(TEXTABLES) $(LOG_FILES) $(AFM_FILES)

OUT_DIST_FILES += $(AFM_FILES)

include $(depth)/make/Stepmake.make 

default: $(ALL_GEN_FILES)


$(outdir)/%.afm $(outdir)/%.ly $(outdir)/%.tex $(outdir)/%.dep: $(outdir)/%.log
	$(PYTHON) $(buildscripts)/mf-to-table.py --package=$(topdir) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep) --ly $(outdir)/$(<F:.log=.ly) --afm $(outdir)/$(<F:.log=.afm) --tex $(outdir)/$(<F:.log=.tex) $<


TEXINSTALL=$(MAKE) INSTALLATION_OUT_DIR=$(TEXDIR)/lilypond depth=.. INSTALLATION_OUT_FILES="$(TEXTABLES)" -f $(stepdir)/Install_outfiles.sub.make $@

AFMINSTALL=$(MAKE) depth=.. INSTALLATION_OUT_DIR=$(datadir) INSTALLATION_OUT_FILES="$(AFM_FILES)" -f $(stepdir)/Install_outfiles.sub.make $@

LYINSTALL=$(MAKE) depth=.. INSTALLATION_OUT_DIR=$(datadir) INSTALLATION_OUT_FILES="$(LYTABLES)" -f $(stepdir)/Install_outfiles.sub.make $@

localinstall: $(ALL_GEN_FILES)
	$(TEXINSTALL)
	$(LYINSTALL)
	$(AFMINSTALL)

localuninstall:
	$(TEXINSTALL)
	$(LYINSTALL)
	$(AFMINSTALL)

INSTALLATION_DIR=$(MFDIR)/public/lilypond
INSTALLATION_FILES=$(MF_FILES)

include $(stepdir)/Installfiles.make

