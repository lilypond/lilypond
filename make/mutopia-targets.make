
.PHONY: mutopia png ps scores tar

all: $(OUT_FILES)

local-WWW: $(ly_examples) $(fly_examples) $(ps_examples) $(png_examples)

local-web:
	$(MAKE) conf=www local-WWW

convert-ly: local-convert-ly
	$(LOOP)

local-convert-ly:
	$(PYTHON) $(CONVERT_LY) -e *ly

tar:
	mkdir -p $(outdir)/$(tarball)
	cp -p *.ly $(outdir)/$(tarball)
	cd $(outdir) && tar czf $(tarball).tar.gz $(tarball)

png: $(png_examples)

ps: $(ps_examples)

scores: $(score_ps)
	$(MAKE) ps_examples="$<" ps

mutopia-letter=$(mutopia-examples:%=out-letter/%.ps.gz)

mutopia:
	$(MAKE) examples="$(mutopia-examples)" PAPERSIZE=letter local-WWW $(mutopia-letter)

local-help:
	@echo -e "\
  convert-ly  convert all LilyPond sources\n\
  mutopia     update PNGs, PostScript a4 and letter of all mutopia-examples\n\
  png         update PNGs of all examples\n\
  ps          update PostScript of all examples\n\
  scores      update PostScript of all scores\n\
"\
#
