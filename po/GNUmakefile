# title	   makefile for po
# file	   po/Makefile

depth = ..

NAME = lilypond
MODULE_NAME = po

POT_FILES = $(call src-wildcard,*.pot)
PO_FILES = $(call src-wildcard,*.po)
MO_FILES = $(addprefix $(outdir)/, $(PO_FILES:.po=.mo))
HELP_CATALOGS = $(PO_FILES:%.po=%)
CATALOGS = $(HELP_CATALOGS:lilypond=)
EXTRA_DIST_FILES = TODO $(PO_FILES) $(POT_FILES)
STEPMAKE_TEMPLATES=podir

include $(depth)/make/stepmake.make

default: $(MO_FILES)

local-install: $(MO_FILES)
	for i in $(CATALOGS); do \
	  $(INSTALL) -d $(DESTDIR)$(localedir)/$$i/LC_MESSAGES; \
	  $(INSTALL) -m 644 $(outdir)/$$i.mo \
	  	     $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/$(package).mo; \
	done

local-uninstall:
	for i in $(CATALOGS); do \
	  rm -f $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/$(package).mo; \
	  rmdir $(DESTDIR)$(localedir)/$$i/LC_MESSAGES || true; \
	done

