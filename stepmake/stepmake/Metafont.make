MF_FILES = $(wildcard *.mf)
FONT_FILES = $(wildcard *[0-9].mf)
EXTRA_DIST_FILES += $(MF_FILES)

$(outdir)/%.dvi: %.mf
	mf $<
	gftodvi  $(basename $<)
	mv   $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.log: %.mf
	mf $<
	mv $(@F) $@
	rm $(basename $< ).*gf

