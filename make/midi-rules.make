.SUFFIXES: .midi

$(outdir)/%.ly:  %.midi
	$(call ly_progress,Making,$@,< midi)
	$(PYTHON) $(MIDI2LY) --quiet -o $(outdir) $<

$(outdir)/%.midi: %.ly $(LILYPOND_BINARY)
	$(call ly_progress,Making,$@,< ly)
	touch $(foreach f, $(HEADER_FIELDS), $(outdir)/$*.$f)
	$(buildscript-dir)/run-and-check "$(LILYPOND_BINARY) $(HEADER_FIELDS:%=-H %) -o $(outdir) $<" "$*.log"
	cp $< $(outdir)

$(outdir)/%-midi.ly: $(outdir)/%.midi $(MIDI2LY)
	$(call ly_progress,Making,$@,< midi)
	(echo '\header {'; for f in $(HEADER_FIELDS); do printf $$f'="'; cat $(outdir)/$*.$$f; echo '"'; done; echo '}') > $(outdir)/$*.header
	$(PYTHON) $(MIDI2LY) $(shell cat $(outdir)/$*.options) --quiet --include-header=$(outdir)/$*.header -o $(outdir) $<

$(outdir)/%.diff: %.ly $(outdir)/%-midi.ly
	$(call ly_progress,Making,$@,)
	$(DIFF) -puN $(MIDI2LY_IGNORE_RES) $^ > $@ || cat $@

$(outdir)/midi.diff: $(OUT_DIFF_FILES)
	$(call ly_progress,Making,$@,(cat))
	cat $(OUT_DIFF_FILES) > $@
