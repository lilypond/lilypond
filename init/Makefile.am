# Automatically generated from ./init/Makefile.am.wild by autowild
# Do not edit
# Generated automatically by wild-perl 0.1
# init/Makefile.am.wild

INIFILES = deutsch.ly dynamic.ly engraver.ly italiano.ly lily-init.ly midi.ly nederlands.ly norsk.ly paper11.ly paper13.ly paper16.ly paper20.ly paper26.ly performer.ly property.ly script.ly simple-init.ly svenska.ly table11.ly table13.ly table16.ly table20.ly table26.ly
EXTRA_DIST = Makefile.am.wild $(INIFILES)

CLEANFILES = 

FONT_FILES = ../mf/feta-beams16.mf ../mf/feta-beams20.mf ../mf/feta-beams26.mf ../mf/feta-black16.mf ../mf/feta-braces16.mf ../mf/feta-braces20.mf ../mf/feta-din10.mf ../mf/feta-nummer10.mf ../mf/feta-nummer12.mf ../mf/feta-nummer3.mf ../mf/feta-nummer4.mf ../mf/feta-nummer5.mf ../mf/feta-nummer6.mf ../mf/feta-nummer7.mf ../mf/feta-nummer8.mf ../mf/feta-test16.mf ../mf/feta-test20.mf ../mf/feta11.mf ../mf/feta13.mf ../mf/feta16.mf ../mf/feta19.mf ../mf/feta20.mf ../mf/feta23.mf ../mf/feta26.mf
TABLES = $(patsubst ../mf/%.mf,%.ly,$(FONT_FILES))

# localclean:
# 	rm -f $(TABLES)

install-data:
	$(INSTALL) -d $(datadir)/lilypond/init
	$(INSTALL) -m 644 $(INIFILES) $(datadir)/lilypond/init

uninstall-data:
	for i in $(INIFILES) ; do rm -f $(datadir)/lilypond/init/$$i; done
	-rmdir $(datadir)/lilypond/init $(datadir)/lilypond/
$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@