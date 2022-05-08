# title	   makefile for po
# file	   po/Makefile

depth = ..

TEMPLATES = podir

include $(depth)/make/lilypond.make

default: $(MO_FILES)

po-update:
	$(foreach i,$(PO_FILES), \
	  $(call ly_progress, Merging, $(i),); \
	  rm -f $(po-outdir)/$(i); \
	  msgmerge $(po-srcdir)/$(i) $(po-outdir)/lilypond.po \
	    -o $(po-outdir)/$(i) && ) true

po-replace: po-update
	mv $(outdir)/lilypond.po lilypond.pot

local-install: $(MO_FILES)
	for i in $(CATALOGS); do \
	  $(INSTALLPY) -d $(DESTDIR)$(localedir)/$$i/LC_MESSAGES; \
	  $(INSTALLPY) -m 644 $(outdir)/$$i/LC_MESSAGES/lilypond.mo \
                       $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/lilypond.mo; \
	done

local-uninstall:
	for i in $(CATALOGS); do \
	  rm -f $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/lilypond.mo; \
	  rmdir $(DESTDIR)$(localedir)/$$i/LC_MESSAGES || true; \
	done
