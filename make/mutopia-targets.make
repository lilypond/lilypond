.PHONY: download mutopia png ps scores tar

.PRECIOUS: $(outdir)/%.ps $(outdir)/%-book.ps
.PRECIOUS: $(outdir)-letter/%.ps


all: $(OUT_FILES)

local-WWW-1: $(ly_examples) $(pdf_examples) $(png_examples)

tar:
	mkdir -p $(outdir)/$(tarball)
	cp -p *.ly $(outdir)/$(tarball)
	cd $(outdir) && tar czf $(tarball).tar.gz $(tarball)
	rm -rf $(outdir)/$(tarball)

png: $(png_examples)

ps: $(ps_examples)

scores: $(score_ps)
	$(MAKE) ps_examples="$<" ps

local-mutopia:
	$(MAKE) examples="$(mutopia-examples)" PAPERSIZE=letter local-WWW-1 $(mutopia-letter)

mutopia: local-mutopia
	$(LOOP)

mutopia-letter=$(mutopia-examples:%=out-letter/%.ps.gz)

local-clean: local-letter-clean

local-letter-clean:
	rm -f $(outdir)-letter/*


local-help: local-mutopia-help

local-mutopia-help:
	@echo -e "\
  <NAME>      update $(outdir)/<NAME>.ps\n\
  <NAME>-book update booklet $(outdir)/<NAME>-book.ps\n\
  mutopia     update PNGs, PostScript a4 and letter of all mutopia-examples\n\
  png         update PNGs of all examples\n\
  ps          update PostScript of all examples\n\
  scores      update PostScript of all scores\n\
"\
#


