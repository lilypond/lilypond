default:

ifeq ($(out),www)
local-WWW-1: $(MASTER_TEXI_FILES) $(PDF_FILES)

local-WWW-2: $(HTML_FILES)
	find $(outdir) -name '*.html' | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/topdocs/$(outdir) NEWS.pdf
	find $(outdir) \( -name 'lily-*.png' -o -name 'lily-*.ly' \) | sed 's!$(outdir)/!!g' | xargs $(buildscript-dir)/mass-link hard $(outdir) $(top-build-dir)/Documentation/topdocs/$(outdir)
endif
