# input/Makefile

depth = ..

SUBDIRS=test bugs

examples=example-3 twinkle-pop praeludium-fuga-E twinkle
flexamples=example-1 example-2 

LOCALSTEPMAKE_TEMPLATES=mutopia


include $(depth)/make/stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))

