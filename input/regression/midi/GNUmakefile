depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc

TEXI2HTML_FLAGS += --nomenu

TMP_FILES = $(sort $(LY_FILES) $(TEXINFO_SOURCES) )
COLLATED_FILES = ${TMP_FILES:%.ly=$(outdir)/recovered/%-midi.ly}


OUT_LY_FILES = $(sort ${LY_FILES:%.ly=$(outdir)/recovered/%-midi.ly} )
OUT_FILES = $(OUT_LY_FILES)
HEADER_FIELDS = texidoc options
OUT_DIFF_FILES = ${LY_FILES:%.ly=$(outdir)/%.diff}
MIDI2LY_IGNORE_RES = -I 'Lily was here' -I '^\\version ' -I 'TEXT_EVENT.*GNU LilyPond' -I '^% included from'
DIFF = diff


include $(depth)/make/stepmake.make

$(outdir)/%.ly:  %.midi
	$(call ly_progress,Making,$@,< midi)
	$(PYTHON) $(MIDI2LY) --quiet -o $(outdir) $<

$(outdir)/%.midi: %.ly $(LILYPOND_BINARY)
	$(call ly_progress,Making,$@,< ly)
	touch $(foreach f, $(HEADER_FIELDS), $(outdir)/$*.$f)
	$(buildscript-dir)/run-and-check.sh "$(LILYPOND_BINARY) $(HEADER_FIELDS:%=-H %) -o $(outdir) $<" "$*.log"
	cp $< $(outdir)

$(outdir)/recovered/%-midi.ly: $(outdir)/%.midi $(MIDI2LY)
	$(call ly_progress,Making,$@,< midi)
	mkdir -p $(dir $@)
	(echo '\header {'; for f in $(HEADER_FIELDS); do printf $$f'="'; cat $(outdir)/$*.$$f; echo '"'; done; echo '}') > $(outdir)/$*.header
	$(PYTHON) $(MIDI2LY) $(shell cat $(outdir)/$*.options) --quiet --include-header=$(outdir)/$*.header -o $@ $<

# We expect that the files to be compared have already been updated by
# running make out=test ....  These prerequisites are not expected to
# be sufficient to automatically retest them now, just to detect when
# there are new results to be compared.
$(outdir)/%.diff: %.ly $(outroot)/out-test/recovered/%-midi.ly
#	no ly_progress here because this is pretty fast and we'll
#	probably log the more interesting "note: ..." below anyway
	-$(DIFF) -puN $(MIDI2LY_IGNORE_RES) $^ > $@

$(outdir)/midi.diff: $(OUT_DIFF_FILES)
#	no ly_progress here because this is pretty fast
	cat $(OUT_DIFF_FILES) > $@
	[ ! -s $@ ] || echo "note: ly->midi->ly differences in $@" 2>&1

##local-test: $(outdir)/midi.diff

check: $(outdir)/midi.diff local-WWW-1 local-WWW-2

test: check


TITLE=MIDI test suite
