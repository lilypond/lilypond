depth = ../..

STEPMAKE_TEMPLATES=documentation texinfo
TO_TOP_FILES=$(addprefix $(outdir)/, $(README_TOP_FILES))
PLAIN_FILES=angels.ly guile.patch

EXTRA_DIST_FILES = $(PLAIN_FILES)


include $(depth)/make/stepmake.make 

default: do-doc dist-plain

$(outdir)/%.html: %.yo
	$(sed-version) < $< > $<.out
	$(YODL2HTML) -doutdir=$(outdir) --live-data=3 -o $@ $<.out
	rm -f $<.out

dist-plain: 
	cp $(PLAIN_FILES) $(outdir)

local-WWW:

