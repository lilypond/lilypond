# title	   makefile for po
# file	   po/Makefile

depth = ..

STEPMAKE_TEMPLATES=podir

include $(depth)/make/stepmake.make

default: $(MO_FILES)

po-update:
	$(foreach i,$(PO_FILES), \
	  rm -f $(po-outdir)/$(i); \
	  msgmerge $(po-srcdir)/$(i) $(po-outdir)/lilypond.po \
	    -o $(po-outdir)/$(i) && ) true

po-replace: po-update
	mv $(outdir)/lilypond.po lilypond.pot

local-install: $(MO_FILES)
	for i in $(CATALOGS); do \
	  $(INSTALL) -d $(DESTDIR)$(localedir)/$$i/LC_MESSAGES; \
	  $(INSTALL) -m 644 $(outdir)/$$i.mo \
	  	     $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/lilypond.mo; \
	done

local-uninstall:
	for i in $(CATALOGS); do \
	  rm -f $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/lilypond.mo; \
	  rmdir $(DESTDIR)$(localedir)/$$i/LC_MESSAGES || true; \
	done
