# input/Makefile

depth = ..

SUBDIRS=test bugs tutorial

examples=praeludium-fuga-E  star-spangled-banner just-friends abc-raff
flexamples=

LOCALSTEPMAKE_TEMPLATES=mutopia


include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))

