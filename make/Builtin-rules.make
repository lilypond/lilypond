#
# project  LilyPond -- the musical typesetter
# title	   cancel all built-in rules
# file	   make/Builtin-rules.make
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

# no assembly sources
%.o : %.s

# no c sources
%.o : %.c

# not using RCS
% :: RCS/%,v
$(outdir)/% :: $(outdir)/RCS/%,v
% : RCS/%,v
$(outdir)/% : $(outdir)/RCS/%,v
%.c : RCS/%,v
%.cc : RCS/%,v
%.hh : RCS/%,v
%.make : RCS/%,v
$(outdir)/%.dep : $(outdir)/RCS/%,v
%.dep : RCS/%.dep,v
$(outdir)/%.dep : $(outdir)/RCS/%.dep,v

# lily knows better
%.tab.c %.tab.h : %.y

# fine suffixes:
Makefile :
%.cc :
%.dep :
$(outdir)/%.dep:
%.hh :
%.make :
%.p :


