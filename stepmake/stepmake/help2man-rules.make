ifneq ($(outdir),./out)
$(outdir)/%.1: out/%.1
	cp $< $@
endif

# We must invoke the generated $(outdir)/help2man script instead of
# the help2man.pl source, which means that the buildscripts directory
# must be built first.
#
# From the perlrun man-page:
#
#	If the #! line does not contain the word "perl", the
#       program named after the #! is executed instead of the Perl
#       interpreter.  This is slightly bizarre, but it helps
#
# Indeed it is.  Perl sucks.
#
# Two screenfulls explaining that the otherwise standard #! is broken
# for perl, and arguing that this broken magic is better in some
# cases.  Four more explaining what a line comment is, and that it may
# be parsed, same here.

HELP2MAN_COMMAND = $(PERL) $(builddir)/buildscripts/$(outbase)/help2man $< > $@

$(outdir)/%.1: $(outdir)/%
	@echo "generating man page from $< --help"
	@echo "$(HELP2MAN_COMMAND)"
	@$(HELP2MAN_COMMAND) || \
	(echo ""; echo "Apparently the man pages failed to build. This is";\
	echo "no problem, since they don't contain any information anyway.";\
	echo "Please run make again, and be prepared for NO manual pages.")
