
$(outdir)/collated-files.tely: $(LY_FILES) $(OUT_LY_FILES)
	$(PYTHON) $(buildscript-dir)/lys-to-tely.py --name=$(outdir)/collated-files --introduction="$(HEADER_FILE)" --footer="$(FOOTER_FILE)" --title="$(TITLE)" $^

