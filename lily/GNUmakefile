
depth = ..

NAME = lilypond
SUBDIRS = include

MODULE_LIBS= $(depth)/flower  $(depth)/ttftool $(depth)/kpath-guile 
MODULE_INCLUDES= $(depth)/flower/include $(depth)/ttftool/include 
MODULE_CXXFLAGS=

HELP2MAN_EXECS = lilypond
STEPMAKE_TEMPLATES= c++ executable po help2man

OUT_DIST_FILES=$(addprefix $(outdir)/,parser.cc lexer.cc parser.hh)

include $(depth)/make/stepmake.make 

ifeq ($(HAVE_LIBKPATHSEA_SO),no)
MODULE_LDFLAGS+= $(KPATHSEA_LIBS)
endif


# for profiling, link guile statically:
#
# USER_LDFLAGS += -static -lltdl -ldl
#

ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *(\([^,]+\)/\1/'
ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *([^"]*"\([^"]+\)"/\1/'

default:

# force these: Make can't know these have to be generated in advance
$(outdir)/lily-parser.o: $(outdir)/parser.hh
$(outdir)/lily-lexer.o: $(outdir)/parser.hh
$(outdir)/lexer.o: $(outdir)/parser.hh $(outdir)/version.hh


$(outdir)/general-scheme.o: $(outdir)/version.hh
$(outdir)/kpath.o: $(outdir)/version.hh
$(outdir)/lily-guile.o: $(outdir)/version.hh
$(outdir)/lily-version.o: $(outdir)/version.hh

local-po: $(outdir)/parser.cc

# ugh.  For --srcdir builds, these must exist to satisfy their broken
# lexer.dep and parser.dep file entries.
ifneq ($(srcdir), .)
.PRECIOUS: $(outdir)/lexer.cc $(outdir)/parser.cc
endif
