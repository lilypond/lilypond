# input/Makefile

depth = ..

SUBDIRS=test bugs tutorial

examples=praeludium-fuga-E  star-spangled-banner just-friends paddy
flexamples=

LOCALSTEPMAKE_TEMPLATES=mutopia
EXTRA_DIST_FILES=paddy.abc

include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))

