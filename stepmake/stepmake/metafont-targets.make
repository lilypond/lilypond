

tfm: $(TFM_FILES)

dvi: $(DVI_FILES)

pks: $(addprefix $(outdir)/, $(XPM_FONTS:%=%.$(XPM_RESOLUTION)pk))

xpms: $(addprefix $(outdir)/, $(XPM_FONTS:%=%.afm)) pks
	$(SHELL) $(depth)/buildscripts/mf-to-xpms.sh $(XPM_FONTS)
