
$(outdir)/$(NAME).tely: $(LY_FILES)
	$(PYTHON) ../../buildscripts/lys-to-tely.py --name=$(outdir)/$(NAME) --title="$(TITLE)" $(LY_FILES)
