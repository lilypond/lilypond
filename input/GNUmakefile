depth = ..

SUBDIRS=test regression tutorial no-notation ascii-art template mutopia 

# nereides is tweaked a lot, can't do (read webpage)

examples= 
#examples+=size11 size13 size16 size20 size23 size26

LOCALSTEPMAKE_TEMPLATES=ly mutopia
EXTRA_DIST_FILES=paddy.abc

include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))


local-WWW-post:
	$(PYTHON) $(buildscript-dir)/mutopia-index.py -o $(builddir)/input/examples.html ./
