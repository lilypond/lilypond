# init/Makefile


# subdir level:
#
depth = ..
#

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


localclean:
	rm -f $(TABLES)


INSTALLATION_DIR=$(datadir)/lilypond/
INSTALLATION_FILES=$(INIFILES)

include $(depth)/make/Installfiles.make

