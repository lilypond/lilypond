depth = ..

SUBDIRS = test regression tutorial no-notation template mutopia 

examples = simple simple-song les-nereides puer-fragment wilhelmus paddy

LOCALSTEPMAKE_TEMPLATES=ly mutopia
EXTRA_DIST_FILES=paddy.abc

include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))


