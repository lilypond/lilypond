# init/Makefile


# subdir level:
#
depth = ..
#

build = ./$(depth)/lily/$(outdir)/.build

# generic stuff/Makefile
#
include ./$(depth)/make/Include.make

# list of distribution files:
# 
INIFILES = $(wildcard *.ly)
DISTFILES = Makefile $(INIFILES)

## we probably can make tex/*defs.tex and init/*.ly in one go...
## moved to mf/Makefile
FONT_FILES = $(wildcard $(depth)/mf/*[0-9].mf)
TABLES = $(patsubst $(depth)/mf/%.mf,%.ly,$(FONT_FILES))
## 
## all: $(TABLES)
## 
## $(outdir)/%.log: $(depth)/mf/%.mf
## 	mf $<
## 	mv $(@F) $@
## 	rm $(shell basename $< .mf).*gf
## 
## %.ly: $(outdir)/%.log
## 	mf-to-table -o $@ $<
## 
localclean:
	rm -f $(TABLES)

localinstall:
	$(INSTALL) -d $(datadir)/lilypond/init
	$(INSTALL) -m 644 $(INIFILES) $(datadir)/lilypond/init

localuninstall:
	for i in $(INIFILES) ; do rm -f $(datadir)/lilypond/init/$$i; done
	-rmdir $(datadir)/lilypond/init $(datadir)/lilypond/
