depth = ../..

NAME = documentation

STEPMAKE_TEMPLATES=documentation  texinfo

TEXTS = $(call src-wildcard,ANNOUNCE-*[0-9]) $(call src-wildcard,CHANGES-*[0-9]) $(call src-wildcard,ChangeLog*[0-9]) $(call src-wildcard,NEWS-*[0-9])
EXTRA_DIST_FILES = $(TEXTS)

include $(depth)/make/stepmake.make

default: local-doc

#urg default local-WWW target uses footify before its time;
# must add footify with txt-to-html target
OUT_TXT_FILES = $(addprefix $(outdir)/, $(addsuffix .txt, $(TEXTS)))
##local-WWW-2: $(OUT_TXT_FILES) $(OUT_HTMLFILES) txt-to-html

copy-for-me:
	$(foreach a, $(README_TOP_FILES), cp ../$(a) $(outdir)/$(a).txt && ) true

$(outdir)/%.txt: %
	cp $< $@

txt-to-html:
	$(PYTHON) $(step-bindir)/text2html.py $(OUT_TXT_FILES)
	$(footify) $(addprefix $(outdir)/, $(addsuffix .html, $(TEXTS)))
