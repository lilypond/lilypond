
depth = ..

NAME = lilypond
SUBDIRS = include

MODULE_LIBS=$(depth)/flower 
MODULE_INCLUDES= $(depth)/flower/include
MODULE_CXXFLAGS= 


HELP2MAN_EXECS = lilypond-bin
STEPMAKE_TEMPLATES= c++ executable po help2man

include $(depth)/make/stepmake.make 

# for profiling, link guile statically:
#
# USER_LDFLAGS += -static -lltdl -ldl
#

ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *(\([^,]+\)/\1/'
ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *([^"]*"\([^"]+\)"/\1/'

default:

# force these: Make can't know these have to be generated in advance
$(outdir)/my-lily-parser.o: $(outdir)/parser.hh
$(outdir)/my-lily-lexer.o: $(outdir)/parser.hh
$(outdir)/lexer.o: $(outdir)/parser.hh $(outdir)/version.hh

$(outdir)/kpath.o: $(outdir)/version.hh
$(outdir)/lily-guile.o: $(outdir)/version.hh
$(outdir)/lily-version.o: $(outdir)/version.hh

# ugh.  For --srcdir builds, these must exist to satisfy their broken
# lexer.dep and parser.dep file entries.
ifneq ($(srcdir), .)
.PRECIOUS: $(outdir)/lexer.cc $(outdir)/parser.cc
endif
