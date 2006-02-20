


$(outdir)/collated-files.tely: $(LY_FILES)
	$(PYTHON) $(buildscript-dir)/lys-to-tely.py --name=$(outdir)/collated-files --title="$(TITLE)" $(LY_FILES)

