# Documentation/man/Makefile

depth = ../..

include $(depth)/make/Stepmake.make 

include $(stepdir)/Documentation.vars.make
include $(stepdir)/Documentation_rules.make

SECTION=1
MANTXT = $(addprefix $(outdir)/, $(addsuffix .txt,$(basename $(TEXINFO_FILES) .texinfo)))
MANGROFF = $(addprefix $(outdir)/, $(addsuffix .$(SECTION),$(basename $(YO_FILES) .yo)))

OUTTEXINFO_FILES= $(addprefix $(outdir)/, $(addsuffix .texinfo,$(basename $(YO_FILES) .yo)))

OUT_DIST_FILES += $(MANGROFF)

default: $(MANGROFF)

texinfo: $(OUTTEXINFO_FILES)

INSTALLATION_OUT_FILES=$(MANGROFF)
INSTALLATION_OUT_DIR=$(mandir)/man$(SECTION)

include $(stepdir)/Install_outfiles.make

local-WWW: $(MANGROFF:.1=.html)
	$(PYTHON) $(step-bindir)/ls-latex.py --package=$(topdir) --title 'Manual pages for LilyPond' $(YO_FILES) \
	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) $(outdir)/index.html
