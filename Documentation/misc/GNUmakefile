depth = ../..

NAME = documentation

STEPMAKE_TEMPLATES=documentation  texinfo

TEXTS =AIMS $(wildcard CHANGES-*[0-9])  $(wildcard ANNOUNCE-*[0-9]) $(wildcard NEWS-*[0-9]) interview
EXTRA_DIST_FILES = $(TEXTS)

include $(depth)/make/stepmake.make 

default: do-doc

local-WWW: $(addprefix $(outdir)/, $(addsuffix .txt, $(TEXTS)))

copy-for-me:
	$(foreach a, $(README_TOP_FILES),cp ../$(a) $(outdir)/$(a).txt && ) true

$(outdir)/%.txt: %
	cp $< $@
