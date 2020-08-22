ifeq ($(out),www)
local-WWW: $(outdir)/collated-files.texi $(outdir)/collated-files.pdf  $(outdir)/collated-files.html
endif

# Set the baseline by renaming the test output directory.
local-test-baseline:
	$(call ly_progress,Moving to,$(outdir)-baseline,from $(notdir $(outdir)))
	rm -rf $(outdir)-baseline
	mv $(outdir) $(outdir)-baseline
#       Redirect to the baseline copy (made elsewhere) of shared files.
	( cd $(outdir)-baseline && \
	  rm -rf share && \
	  ln -sf ../$(depth)/out-baseline/share )

# This recipe creates the same content regardless of the subdir it
# runs in, so it needs to be run in only one subdir to serve its
# purpose.  Any more would be noise in the test output.
lysdoc-gittxt:
	if test -d $(top-src-dir)/.git  ; then \
		cd $(top-src-dir) ; \
		BR=`LANG=c git branch | grep "^\*" | sed -e "s|^* *||"` ; \
		HD=`git rev-parse --verify HEAD` ; \
		FP=`git merge-base --octopus origin/master HEAD` ; \
		echo "    BRANCH: $$BR" ; \
		echo "      HEAD: $$HD" ; \
		if [ ! -z $$FP ]; then  \
			echo "MERGE_BASE: $$FP" ; \
			echo ; \
			echo "   HISTORY:" ; \
			echo "   ========" ; \
			git log --pretty=format:"      HASH: %H%n   SUBJECT: %s%n" $$FP~1..HEAD ; \
		else \
			echo "MERGE_BASE: unknown" ; \
			echo ; \
			echo "   HISTORY:" ; \
			echo "   ========" ; \
			git log --max-count=10 --pretty=format:"      HASH: %H%nSUBJECT: %s%n" ; \
		fi ; \
		echo "" ; \
		date ; \
	fi > $(outdir)/tree.gittxt

lysdoc-test:
#       Creating collated-files.texi also produces the tested output.
#       Removing it works around incomplete dependencies.
	rm -f $(outdir)/collated-files.texi
	$(MAKE) LILYPOND_BOOK_LILYPOND_FLAGS="-dbackend=eps --formats=ps $(LILYPOND_JOBS) -dseparate-log-files -dinclude-eps-fonts -dgs-load-fonts --header=texidoc -I $(top-src-dir)/Documentation/included/ -dcheck-internal-types -ddump-signatures -danti-alias-factor=1" LILYPOND_BOOK_WARN= $(outdir)/collated-files.html LYS_OUTPUT_DIR=$(top-build-dir)/out/lybook-testdb
#       Later testing will find fonts via this link.
	( cd $(outdir) && \
	  rm -rf share && \
	  ln -sf ../$(depth)/out/share )
