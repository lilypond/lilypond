

all: $(OUT_FILES)

local-WWW: $(ly_examples) $(fly_examples) $(ps_examples) $(png_examples)
#	(cd $(outdir); $(PYTHON) ../$(buildscript-dir)/mutopia-index.py --prefix=../ --suffix=/$(outdir) $(html_subdirs) $(all_examples))
#	$(footify) $(outdir)/index.html

local-web:
	$(MAKE) conf=www local-WWW

convert-ly: local-convert-ly
	$(LOOP)

local-convert-ly:
	$(PYTHON) $(script-dir)/convert-ly.py -e *ly

tar:
	mkdir -p $(outdir)/$(tarball)
	cp -p *.ly $(outdir)/$(tarball)
	cd $(outdir) && tar czf $(tarball).tar.gz $(tarball)
