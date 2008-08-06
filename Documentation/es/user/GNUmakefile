ISOLANG = es
depth = ../../..
LOCALSTEPMAKE_TEMPLATES = lilypond ly doclang
include $(depth)/make/stepmake.make


local-WWW-1: $(XREF_MAP_FILES) $(outdir)/lilypond-snippets.xref-map \
	$(outdir)/music-glossary.xref-map \
	$(outdir)/lilypond-internals.xref-map

$(outdir)/lilypond-snippets.xref-map:
	ln -sf ../../../../input/lsr/$@ $@

$(outdir)/lilypond-internals.xref-map $(outdir)/music-glossary.xref-map:
	ln -sf ../../../user/$@ $@
