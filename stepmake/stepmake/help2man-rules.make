# ugh. 
# Man-page:
#	If the #! line does not contain the word "perl", the
#       program named after the #! is executed instead of the Perl
#       interpreter.  This is slightly bizarre, but it helps
# Indeed it is. Perl sucks.
#
$(outdir)/%.1: $(outdir)/%
	$(PERL) $(depth)/buildscripts/$(outdir)/help2man $< > $@
