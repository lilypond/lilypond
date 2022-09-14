depth = ../..

TEMPLATES = lilypond podir

include $(depth)/make/lilypond.make

doc-localedir=$(outdir)
DOC_PO_SOURCES = python/auxiliar/postprocess_html.py \
 scripts/auxiliar/tely-gettext.py
TELY_FILES = $(shell ls $(depth)/Documentation/*.tely)
TELY_FILES += $(foreach l, $(LANGS), $(shell ls $(depth)/Documentation/$(l)/*.tely))

messages: $(outdir)/messages

$(outdir)/messages: $(MO_FILES)
	$(call ly_progress,Making,$@,(hard links))
	for i in $(CATALOGS); do \
	  mkdir -p $(doc-localedir)/$$i/LC_MESSAGES; \
	  $(LN) -f $(outdir)/$$i/LC_MESSAGES/lilypond.mo $(doc-localedir)/$$i/LC_MESSAGES/lilypond-doc.mo; \
	done
	touch $@

po-update:
	$(PYTHON) $(auxscript-dir)/texi-langutils.py -d $(outdir) -o texi.pot --gettext $(TELY_FILES:%.tely=../%.tely)
	$(foreach i, $(DOC_PO_SOURCES), cp $(top-src-dir)/$(i) $(outdir) &&) true
	cd $(outdir) && xgettext --keyword=_doc -cjn -L Python -o buildscripts.pot $(foreach i, $(DOC_PO_SOURCES), $(notdir $(i)))
	msgcat -o $(outdir)/doc.pot $(outdir)/buildscripts.pot $(outdir)/texi.pot
	msgmerge -U lilypond-doc.pot $(outdir)/doc.pot
	for i in $(PO_FILES); do \
	  msgmerge -U $$i lilypond-doc.pot; \
	done
