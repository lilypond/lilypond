

all: $(OUT_FILES)


local-WWW: $(ly_examples) $(fly_examples) $(ps_examples) $(png_examples)
	(cd $(outdir); $(PYTHON) ../$(buildscripts)/mutopia-index.py --prefix=../ --suffix=/$(outdir) $(html_subdirs) $(all_examples))
	echo $^ > $(depth)/wwwlist
#	$(footify) $(outdir)/index.html

convert-mudela: local-convert-mudela
	$(LOOP)

local-convert-mudela:
	$(PYTHON) $(depth)/scripts/convert-mudela.py -e *ly
