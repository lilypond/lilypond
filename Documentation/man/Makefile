
depth = ../..

include $(depth)/make/Include.make
include $(depth)/make/Docrules.make
SECTION=1
MANGROFF = $(addprefix $(outdir)/, $(addsuffix .$(SECTION),$(basename $(PODFILES) .pod)))

default: $(MANGROFF)


INSTALLATION_OUT_FILES=$(MANGROFF)
INSTALLATION_DIR=$(mandir)/man$(SECTION)

include $(depth)/make/Install_outfiles.make
