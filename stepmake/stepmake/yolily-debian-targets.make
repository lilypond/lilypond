# Yolily_Debian_targets.make

default: $(OUTCONF_FILES)

$(outdir)/%: %
	rm -f $@
	ln $< $@

$(outdir)/control: $(addprefix $(outdir)/, $(BLURBS))

$(outdir)/%: $(doc-dir)/%.in
	rm -f $@
	cat $< | sed 's%^% %' > $@

localdist: default
