# input/Makefile

depth = ..

SUBDIRS=bugs tricks test regression tutorial no-notation ascii-art

examples=trip  star-spangled-banner paddy scarlatti-test
flexamples=

LOCALSTEPMAKE_TEMPLATES=mutopia
EXTRA_DIST_FILES=paddy.abc

include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))

