
$(outdir)/%.1: $(outdir)/%
	$(PERL) $(depth)/buildscripts/help2man.pl $< > $@
