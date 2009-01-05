# We must invoke the generated $(outdir)/help2man script instead of
# the help2man.pl source, which means that the scripts/build directory
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

HELP2MAN_COMMAND = $(buildscript-dir)/help2man $< > $@

ifeq ($(strip $(CROSS)),no)
$(outdir)/%.1: $(outdir)/% $(buildscript-dir)/help2man
	$(HELP2MAN_COMMAND)
else
# When cross building, some manpages will not build because the
# executable does not run.  We assume the manpages to be generated
# during a previous compile for the build host, with config=for-build,
# in the directory $(outdir)-for-build.
$(outdir)/%.1: $(outdir:%=%-for-build)/%.1
	cp $< $@
endif

ifneq ($(outdir),./out)
$(outdir)/%.1: out/%.1
	cp $< $@
endif

$(buildscript-dir)/help2man:
	$(MAKE) -C $(depth)/scripts/build
