
depth = ../..

include $(depth)/make/Include.make
include $(depth)/make/Docrules.make
SECTION=1
MANTXT = $(addprefix $(outdir)/, $(addsuffix $(DOTTEXT),$(basename $(PODFILES) .pod)))
MANGROFF = $(addprefix $(outdir)/, $(addsuffix .$(SECTION),$(basename $(PODFILES) .pod)))

default: $(MANTXT) $(MANGROFF)


INSTALLATION_OUT_FILES=$(MANGROFF)
INSTALLATION_OUT_DIR=$(mandir)/man$(SECTION)

include $(depth)/make/Install_outfiles.make

local-WWW: $(OUTPODFILES:.pod=.html)
	ls-latex  --title 'Manual pages for LilyPond' $(PODFILES)  > $(outdir)/index.html
