# debian/Makefile

depth = ..

NAME = debian
at-dir = $(outdir)/
at-ext =

CONF_FILES = changelog control copyright emacsen-startup\
 preinst rules watch

EXTRA_DIST_FILES = $(CONF_FILES)\
 README.Debian\
 TODO\
 compat\
 control.foka\
 control.potato\
 lilypond-data.dirs\
 lilypond-data.doc-base\
 lilypond-data.emacsen-startup\
 lilypond-data.install\
 lilypond-data.postinst\
 lilypond-data.postrm\
 lilypond-doc.README.Debian\
 lilypond-doc.dirs\
 lilypond-doc.doc-base\
 lilypond-doc.postinst\
 lilypond-doc.prerm\
 lilypond.doc-base\
 lilypond.install\
 lilypond1.3.copyright\
 #


#OUTCONF_FILES = $(addprefix $(outdir)/, $(basename $(CONF_FILES))) $(OUTIN_FILES)
OUTCONF_FILES = $(OUTIN_FILES)
OUT_DIST_FILES = $(OUTIN_FILES)

STEPMAKE_TEMPLATES = debian
include $(depth)/make/stepmake.make 

