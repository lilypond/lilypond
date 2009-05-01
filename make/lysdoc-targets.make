ifeq ($(out),www)
local-WWW-1: $(outdir)/collated-files.texi $(outdir)/collated-files.pdf

local-WWW-2: $(outdir)/collated-files.html
endif

local-test-baseline:
	rm -rf $(outdir)-baseline
	mv $(outdir) $(outdir)-baseline

local-test:
	rm -f $(outdir)/collated-files.html
	if test -d $(top-src-dir)/.git  ; then \
		echo -e 'HEAD is:\n\n\t' ; \
		(cd $(top-src-dir) && git log --max-count=1 --pretty=oneline ) ;\
		echo -e '\n\n\n' ; \
		(cd $(top-src-dir) && git diff ) ; \
	fi > $(outdir)/tree.gittxt
	$(MAKE) LILYPOND_BOOK_LILYPOND_FLAGS="-dbackend=eps --formats=ps $(LILYPOND_JOBS) -dseparate-log-files -dinclude-eps-fonts -dgs-load-lily-fonts --header=texidoc -I $(top-src-dir)/input/manual -ddump-profile -dcheck-internal-types -ddump-signatures -danti-alias-factor=1" LILYPOND_BOOK_VERBOSE= $(outdir)/collated-files.html LYS_OUTPUT_DIR=$(top-build-dir)/out/lybook-testdb
	rsync -L -a --exclude 'out-*' --exclude 'out' --exclude mf --exclude source --exclude mf $(top-build-dir)/out/share $(outdir)

