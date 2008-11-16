$(outdir)/collated-files.tely: $(LY_FILES) $(OUT_LY_FILES)
	$(PYTHON)  $(LYS_TO_TELY) --name=$(outdir)/collated-files.tely --title="$(TITLE)" $^

