$(outdir)/collated-files.tely: $(LY_FILES) $(OUT_LY_FILES)
	$(PYTHON) $(buildscript-dir)/lys-to-tely.py --name=$(outdir)/collated-files.tely --title="$(TITLE)" $^

