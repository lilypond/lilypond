# title	   makefile for po
# file	   po/Makefile 

depth = ..

NAME = lilypond
MODULE_NAME = po

PO_FILES = $(wildcard *.po)
MO_FILES = $(addprefix $(outdir)/, $(PO_FILES:.po=.mo))
HELP_CATALOGS = $(PO_FILES:%.po=%)
CATALOGS = $(HELP_CATALOGS:lilypond=)
EXTRA_DIST_FILES = TODO $(PO_FILES)

include $(depth)/make/Stepmake.make 
include $(stepdir)/Po_rules.make

default: $(MO_FILES)

localinstall: $(MO_FILES)
	for i in $(CATALOGS); do \
	  $(INSTALL) -d $(localedir)/$$i/LC_MESSAGES; \
	  $(INSTALL) -m 644 $(outdir)/$$i.mo \
	  $(localedir)/$$i/LC_MESSAGES/$(package).mo; \
	done

localuninstall:
	for i in $(CATALOGS); do \
	  rm -f $(localedir)/$$i/LC_MESSAGES/$(package).mo; \
	  rmdir $(localedir)/$$i/LC_MESSAGES; \
	done

