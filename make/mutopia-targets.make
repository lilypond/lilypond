
.PHONY: mutopia png ps scores tar

.PRECIOUS: $(outdir)/%.ps $(outdir)/%-book.ps

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
	rm -rf $(outdir)/$(tarball)

png: $(png_examples)

ps: $(ps_examples)

scores: $(score_ps)
	$(MAKE) ps_examples="$<" ps

mutopia-letter=$(mutopia-examples:%=out-letter/%.ps.gz)

mutopia:
	$(MAKE) examples="$(mutopia-examples)" PAPERSIZE=letter local-WWW $(mutopia-letter)

#
# <NAME> and -book targets only available through ly.make template makefile;
# too scary to install in LilyPonds make yet.
#
#

ifeq (0,1)
#
# Timothy's booklet
#
$(outdir)/%-book.ps: $(outdir)/%.ps
	psbook $< $<.1
	pstops '2:0L(11.45in,0.25in)+1L(11.45in,5.6in)' $<.1 $@
	rm -f $<.1

#
# Catch-all target: type `make foo' to make out/foo.ps,
# or make `foo-book' to make out/foo-book.ps
#
%: $(outdir)/%.ps
	@echo Making $@ from $<
endif

local-help:
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
