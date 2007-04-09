ISOLANG = fr
depth = ../..
SUBDIRS = user
STEPMAKE_TEMPLATES = documentation

include $(depth)/make/stepmake.make

local-WWW: lang-merge

lang-merge: $(outdir)/index.html
	ln -f $(outdir)/index.html $(depth)/Documentation/$(outdir)/index.$(ISOLANG).html
