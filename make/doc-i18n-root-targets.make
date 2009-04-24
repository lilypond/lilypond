default:

ifeq ($(out),www)
local-WWW-2: $(OUT_HTML_FILES)
	$(buildscript-dir)/mass-link --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/$(outdir) $(HTML_FILES)
endif
