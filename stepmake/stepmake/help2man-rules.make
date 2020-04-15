ifeq ($(strip $(CROSS)),no)
$(outdir)/%.1: $(outdir)/% $(buildscript-dir)/help2man.pl
	$(call ly_progress,Making,$@,)
	$(PERL) $(buildscript-dir)/help2man.pl $< > $@
endif
