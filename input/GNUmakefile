# input/Makefile

depth = ..

SUBDIRS=bugs test regression tutorial no-notation ascii-art

examples=trip  star-spangled-banner paddy twinkle
flexamples=

LOCALSTEPMAKE_TEMPLATES=ly mutopia
EXTRA_DIST_FILES=paddy.abc

include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))

