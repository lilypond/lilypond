# input/Makefile

depth = ..

SUBDIRS=bugs features test regression tutorial

examples=trip  star-spangled-banner paddy scarlatti-test
flexamples=

LOCALSTEPMAKE_TEMPLATES=mutopia
EXTRA_DIST_FILES=paddy.abc

include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))

