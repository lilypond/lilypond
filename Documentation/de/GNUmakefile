ISOLANG = de
depth = ../..
SUBDIRS = user
STEPMAKE_TEMPLATES = documentation

include $(depth)/make/stepmake.make

local-WWW: $(outdir)/index.html lang-merge

lang-merge:
	ln -f $(outdir)/index.html $(depth)/Documentation/$(outdir)/index.$(ISOLANG).html
