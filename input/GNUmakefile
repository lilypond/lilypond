depth = ..

SUBDIRS=test regression tutorial no-notation ascii-art template mutopia 

examples= les-nereides

LOCALSTEPMAKE_TEMPLATES=ly mutopia
EXTRA_DIST_FILES=paddy.abc

include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))


local-WWW-post:
	$(PYTHON) $(buildscript-dir)/mutopia-index.py -o $(builddir)/input/examples.html ./
