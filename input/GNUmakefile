depth = ..

SUBDIRS=bugs test regression tutorial no-notation ascii-art template mutopia

# nereides is tweaked a lot, can't do (read webpage)
examples=trip star-spangled-banner paddy twinkle

LOCALSTEPMAKE_TEMPLATES=ly mutopia
EXTRA_DIST_FILES=paddy.abc

include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))

$(outdir)/nereides.ly: les-nereides.ly
	cpp -P -traditional -o $@ -DFAKE_GRACE $<


