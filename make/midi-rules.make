.SUFFIXES: .midi

$(outdir)/%.ly:  %.midi
	$(call ly_progress,Making,$@,< midi)
	$(PYTHON) $(MIDI2LY) --quiet -o $(outdir) $<

$(outdir)/%.midi: %.ly $(LILYPOND_BINARY)
	$(call ly_progress,Making,$@,< ly)
	touch $(foreach f, $(HEADER_FIELDS), $(outdir)/$*.$f)
	$(buildscript-dir)/run-and-check "$(LILYPOND_BINARY) $(HEADER_FIELDS:%=-H %) -o $(outdir) $<" "$*.log"
	cp $< $(outdir)

$(outdir)/recovered/%-midi.ly: $(outdir)/%.midi $(MIDI2LY)
	$(call ly_progress,Making,$@,< midi)
	mkdir -p $(dir $@)
	(echo '\header {'; for f in $(HEADER_FIELDS); do printf $$f'="'; cat $(outdir)/$*.$$f; echo '"'; done; echo '}') > $(outdir)/$*.header
	$(PYTHON) $(MIDI2LY) $(shell cat $(outdir)/$*.options) --quiet --include-header=$(outdir)/$*.header -o $@ $<

$(outdir)/%.diff: %.ly $(outdir)/recovered/%-midi.ly
#	no ly_progress here because this is pretty fast and we'll
#	probably log the more interesting "note: ..." below anyway
	-$(DIFF) -puN $(MIDI2LY_IGNORE_RES) $^ > $@

$(outdir)/midi.diff: $(OUT_DIFF_FILES)
#	no ly_progress here because this is pretty fast
	cat $(OUT_DIFF_FILES) > $@
	[ ! -s $@ ] || echo "note: ly->midi->ly differences in $@" 2>&1
