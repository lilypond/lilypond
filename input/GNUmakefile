# input/Makefile

depth = ..

SUBDIRS=test bugs

examples=praeludium-fuga-E  twinkle-pop
flexamples=

LOCALSTEPMAKE_TEMPLATES=mutopia


include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))

