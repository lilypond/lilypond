# Documentation/man/Makefile

depth = ../..

STEPMAKE_TEMPLATES=documentation
SECTION=1
MANTXT = $(addprefix $(outdir)/, $(addsuffix .txt,$(basename $(TEXINFO_FILES) .texinfo)))
MANGROFF = $(addprefix $(outdir)/, $(addsuffix .$(SECTION),$(basename $(YO_FILES) .yo)))

OUT_DIST_FILES += $(MANGROFF)

include $(depth)/make/stepmake.make 

default: $(MANGROFF)

INSTALLATION_OUT_FILES=$(MANGROFF)
INSTALLATION_OUT_DIR=$(mandir)/man$(SECTION)

include $(stepdir)/install-outfiles.make

local-WWW: $(MANGROFF:.1=.html)
	$(PYTHON) $(step-bindir)/ls-latex.py --package=$(topdir) --title 'Manual pages for LilyPond' $(YO_FILES) \
	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) $(outdir)/index.html
