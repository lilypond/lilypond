# ugh. 
# Man-page:
#	If the #! line does not contain the word "perl", the
#       program named after the #! is executed instead of the Perl
#       interpreter.  This is slightly bizarre, but it helps
# Indeed it is. Perl sucks.
#

ifneq ($(outdir),./out)
$(outdir)/%.1: out/%.1
	cp $< $@
endif

$(outdir)/%.1: $(outdir)/%
	echo "generating man page from --help"
	@$(PERL) $(builddir)/buildscripts/$(outbase)/help2man $< > $@ || \
	(echo ""; echo "Apparently the man pages failed to build. This is";\
	echo "no problem, since they don't contain any information anyway.";\
	echo "Please run make again, and be prepared for NO manual pages.")
