
$(outdir)/%: %.pl $(builddir)/config.make
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.bash $(builddir)/config.make
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.scm $(builddir)/config.make
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.expect $(builddir)/config.make
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.sh $(builddir)/config.make
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

$(outdir)/%: %.py $(builddir)/config.make
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

