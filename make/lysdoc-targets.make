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

lysdoc-test:
#       Creating collated-files.texi also produces the tested output.
#       Removing it works around incomplete dependencies.
	rm -f $(outdir)/collated-files.texi
	$(MAKE) LILYPOND_BOOK_LILYPOND_FLAGS=" \
-dseparate-page-formats=ps $(LILYPOND_JOBS) -dseparate-log-files \
-ddeterministic \
-dinclude-eps-fonts -dgs-load-fonts --header=texidoc \
-I $(top-src-dir)/Documentation/included/ -dcheck-internal-types \
-danti-alias-factor=1 \
$(LOCAL_LILYPOND_FLAGS)" \
		LILYPOND_BOOK_WARN= \
		LYS_OUTPUT_DIR=$(top-build-dir)/out/lybook-testdb \
		$(outdir)/collated-files.html
#       Later testing will find fonts via this link.
	( cd $(outdir) && \
	  rm -rf share && \
	  ln -sf ../$(depth)/out/share )
