# input/Makefile

depth = ..

SUBDIRS=test bugs

examples=example-3 twinkle-pop praeludium-fuga-E twinkle
flexamples=example-1 example-2

LOCALMAKEFILES=Mutopia


include $(depth)/make/Stepmake.make 

local-check: $(addsuffix .dvi, $(addprefix $(outdir)/, $(examples)))

