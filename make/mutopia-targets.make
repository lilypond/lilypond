
include $(stepdir)/www-targets.make


.PHONY: download mutopia png ps scores tar

.PRECIOUS: $(outdir)/%.ps $(outdir)/%-book.ps
.PRECIOUS: $(outdir)-letter/%.dvi $(outdir)-letter/%.ps


all: $(OUT_FILES)

local-WWW: $(ly_examples) $(fly_examples) $(ps_examples) $(png_examples)

convert-ly: local-convert-ly
	$(LOOP)

local-convert-ly:
	$(PYTHON) $(CONVERT_LY) --edit --assume-old *ly

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
	$(MAKE) examples="$(mutopia-examples)" PAPERSIZE=letter local-WWW $(mutopia-letter)

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
  convert-ly  convert all LilyPond sources\n\
  mutopia     update PNGs, PostScript a4 and letter of all mutopia-examples\n\
  png         update PNGs of all examples\n\
  ps          update PostScript of all examples\n\
  scores      update PostScript of all scores\n\
"\
#


