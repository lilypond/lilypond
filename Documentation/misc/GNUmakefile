depth = ../..

NAME = documentation

STEPMAKE_TEMPLATES=documentation  texinfo

TEXTS =AIMS $(wildcard CHANGES-*[0-9])  $(wildcard ANNOUNCE-*[0-9]) $(wildcard NEWS-*[0-9]) interview fontinstallation
EXTRA_DIST_FILES = $(TEXTS)

include $(depth)/make/stepmake.make 

default: local-doc

#urg default local-WWW target uses footify before its time; 
# must add footify with txt-to-html target
OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(TEXTS)))
local-WWW: $(OUT_TXT_FILES) $(OUT_HTMLFILES) txt-to-html

copy-for-me:
	$(foreach a, $(README_TOP_FILES), cp ../$(a) $(outdir)/$(a).txt && ) true

$(outdir)/%.txt: %
	cp $< $@

txt-to-html:
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)
	$(footify) $(addprefix $(outdir)/, $(addsuffix .html, $(TEXTS)))
