# mf/Makefile

depth = ..

STEPMAKE_TEMPLATES=metafont metapost asciifont install install-out

AF_FILES = $(wildcard *.af) 

EXTRA_DIST_FILES += README feta.tex

FET_FILES = $(wildcard feta[0-9]*.mf)
FONT_FILES = $(wildcard feta*[0-9].mf)
XPM_FONTS = feta20

LOG_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.log))
TEXTABLES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.tex))
AFM_FILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.afm) $(AF_FILES:.af=.afm))
TFM_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.tfm))
PFA_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.pfa)) 

ALL_GEN_FILES= $(TEXTABLES) $(LOG_FILES) $(AFM_FILES) $(TFM_FILES) $(PFA_FILES)

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
include $(depth)/make/stepmake.make 

default: $(ALL_GEN_FILES)

$(outdir)/%.afm  $(outdir)/%.tex $(outdir)/%.dep: $(outdir)/%.log
	$(PYTHON) $(buildscripts)/mf-to-table.py --package=$(topdir) --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep)  --afm $(outdir)/$(<F:.log=.afm) --tex $(outdir)/$(<F:.log=.tex) $<

localclean:
	rm -f mfplain.mem mfplain.log mpout.log
