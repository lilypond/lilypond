depth = ../..

STEPMAKE_TEMPLATES=documentation texinfo
TO_TOP_FILES=$(addprefix $(outdir)/, $(README_TOP_FILES))
PLAIN_FILES=angels.ly guile.patch

EXTRA_DIST_FILES = $(PLAIN_FILES)


include $(depth)/make/stepmake.make 

default: local-doc dist-plain

$(outdir)/%.texi: %.texi
	m4 -DTOPLEVEL_VERSION=$(TOPLEVEL_VERSION) < $< > $@ 

dist-plain: 
	cp $(PLAIN_FILES) $(outdir)

local-WWW:

