depth = ../../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc musicxml

TITLE=Lilypond musicxml2ly Regression Tests


ILY_FILES = $(call src-wildcard,*.ily)
EXTRA_OUT_LY_FILES = ${ILY_FILES:%.ily=$(outdir)/%.ly}


include $(depth)/make/stepmake.make
TITLE=Lilypond musicxml2ly Regression Tests

.SUFFIXES: .ily
$(outdir)/%.ly: %.ily
	ln -f $< $@
