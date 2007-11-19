# title	   makefile for po
# file	   po/Makefile

depth = ..

NAME = lilypond
MODULE_NAME = po
DOMAIN = $(NAME)
EXTRA_DIST_FILES = TODO README
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

