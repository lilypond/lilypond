# Automatically generated from ./tex/Makefile.am.wild by autowild
# Do not edit
# Generated automatically by wild-perl 0.1
# tex/Makefile.am.wild

TEXFILES = dyndefs.tex eglerdefs.tex fetdefs.tex lily-mf-defs.tex lily-ps-defs.tex lilyponddefs.tex ltest.tex taupindefs.tex titledefs.tex
EXTRA_DIST = Makefile.am.wild $(TEXFILES)

CLEANFILES = 

install-data:
	$(INSTALL) -d $(TEXDIR)/lilypond
	$(INSTALL) -m 644 $(TEXFILES) $(TEXDIR)/lilypond/

uninstall-data:
	for i in $(TEXFILES) ; do rm -f $(TEXDIR)/lilypond/$$i; done
	-rmdir $(TEXDIR)/lilypond/
$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@