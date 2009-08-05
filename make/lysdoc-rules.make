$(outdir)/collated-files.tely: $(COLLATED_FILES)
	$(LYS_TO_TELY) --name=$(outdir)/collated-files.tely --title="$(TITLE)" $^
