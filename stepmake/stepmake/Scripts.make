# Scripts.make


include $(stepdir)/Script_files.make

all: $(PERL_SCRIPTS) $(PYTHON_SCRIPTS) $(SH_SCRIPTS)

$(outdir)/%: %.pl
	cat $< | $(sed-atvariables) > $@
	chmod 755 $@

#FIXME.  Check for bash?
$(outdir)/%: %.sh
	cat $< | $(sed-atvariables) > $@
	chmod 755 $@


$(outdir)/%: %.py
	cat $< | $(sed-atvariables) > $@
	chmod 755 $@

