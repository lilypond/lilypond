ifeq ($(out),www)
local-WWW-1: $(outdir)/collated-files.texi $(outdir)/collated-files.pdf

local-WWW-2: $(outdir)/collated-files.html
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

lysdoc-test:
#       Creating collated-files.texi also produces the tested output.
#       Removing it works around incomplete dependencies.
	rm -f $(outdir)/collated-files.texi
	if test -d $(top-src-dir)/.git  ; then \
		cd $(top-src-dir) ; \
		BR=`LANG=c git branch | grep "^\*" | sed -e "s|^* *||"` ; \
		HD=`git rev-parse --verify HEAD` ; \
		FP=`git merge-base --octopus origin/master HEAD` ; \
		echo "    BRANCH: $$BR" ; \
		echo "      HEAD: $$HD" ; \
		if [ ! -z $$FP ]; then  \
			echo "MERGE_BASE: $$FP" ; \
			echo -e '\n   HISTORY:\n   ========\n' ; \
			git log --pretty=format:"      HASH: %H%n   SUBJECT: %s%n" $$FP~1..HEAD ; \
		else \
			echo "MERGE_BASE: unknown" ; \
			echo -e '\n   HISTORY:\n   ========\n' ; \
			git log --max-count=10 --pretty=format:"      HASH: %H%nSUBJECT: %s%n" ; \
		fi ; \
		echo "" ; \
	fi > $(outdir)/tree.gittxt
ifeq ($(USE_EXTRACTPDFMARK),yes)
	$(MAKE) LILYPOND_BOOK_LILYPOND_FLAGS="-dbackend=eps --formats=ps $(LILYPOND_JOBS) -dseparate-log-files -dinclude-eps-fonts -dgs-load-fonts --header=texidoc -I $(top-src-dir)/Documentation/included/ -ddump-profile -dcheck-internal-types -ddump-signatures -danti-alias-factor=1 -dfont-export-dir=$(top-build-dir)/out-fonts -O TeX-GS" LILYPOND_BOOK_WARN= $(outdir)/collated-files.html LYS_OUTPUT_DIR=$(top-build-dir)/out/lybook-testdb
else
	$(MAKE) LILYPOND_BOOK_LILYPOND_FLAGS="-dbackend=eps --formats=ps $(LILYPOND_JOBS) -dseparate-log-files -dinclude-eps-fonts -dgs-load-lily-fonts --header=texidoc -I $(top-src-dir)/Documentation/included/ -ddump-profile -dcheck-internal-types -ddump-signatures -danti-alias-factor=1" LILYPOND_BOOK_WARN= $(outdir)/collated-files.html LYS_OUTPUT_DIR=$(top-build-dir)/out/lybook-testdb
endif
#       Later testing will find fonts via this link.
	( cd $(outdir) && \
	  rm -rf share && \
	  ln -sf ../$(depth)/out/share )
