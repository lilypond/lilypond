depth = ../..

STEPMAKE_TEMPLATES=podir

include $(depth)/make/stepmake.make

doc-localedir=$(outdir)
DOMAIN=lilypond-doc
DOC_PO_SOURCES = add_html_footer.py langdefs.py

messages: $(MO_FILES)
	for i in $(CATALOGS); do \
	  mkdir -p $(doc-localedir)/$$i/LC_MESSAGES; \
	  cp $(outdir)/$$i.mo $(doc-localedir)/$$i/LC_MESSAGES/$(DOMAIN).mo; \
	done

po-update:
	$(PYTHON) $(buildscript-dir)/texi-langutils.py -d $(outdir) -o texi.pot --gettext ../$(depth)/Documentation/user/lilypond.tely
	xgettext -cjn -L Python -o $(outdir)/buildscripts.pot $(foreach i, $(DOC_PO_SOURCES), $(buildscript-dir)/$(i) )
	msgcat -o $(outdir)/doc.pot $(outdir)/buildscripts.pot $(outdir)/texi.pot
	msgmerge -U lilypond-doc.pot $(outdir)/doc.pot
	for i in $(CATALOGS); do \
	  msgmerge -U $$i.po lilypond-doc.pot; \
	done
