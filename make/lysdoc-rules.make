


$(outdir)/$(NAME).tely: $(LY_FILES)
	$(PYTHON) ../../buildscripts/lys-to-tely.py --name=$(outdir)/collated-files.html --title="$(TITLE)" $(LY_FILES)

