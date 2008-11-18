
depth = ..

NAME = lilypond
SUBDIRS = include

MODULE_LIBS=$(depth)/flower
MODULE_INCLUDES= $(depth)/flower/include

# need this to convert between function pointers and member function pointers.
MODULE_CXXFLAGS= -Wno-pmf-conversions 


HELP2MAN_EXECS = lilypond
STEPMAKE_TEMPLATES=c c++ executable po help2man

# list parser.hh first: making parser.hh removes parser.cc
OUT_DIST_FILES=$(addprefix $(outdir)/,parser.hh parser.cc)

include $(depth)/make/stepmake.make

ifeq ($(HAVE_LIBKPATHSEA_SO),no)
MODULE_LDFLAGS += $(KPATHSEA_LIBS)
endif
ifeq ($(LINK_GXX_STATICALLY),yes)
MODULE_LDFLAGS += -L$(outdir) -static-libgcc
endif

CXXFLAGS += -Woverloaded-virtual


# for profiling, link guile statically:
#
# CONFIG_LDFLAGS += -static -lltdl -ldl
#

ifeq ($(PLATFORM_WINDOWS),yes)
WINDRES_FLAGS += -DLilyPond=0 -DLY=1
O_FILES += $(outdir)/lilypond.rc.o
$(outdir)/lilypond: $(outdir)/lilypond.rc.o

$(outdir)/lilypond.rc.o: $(outdir)/lilypond.ico $(outdir)/ly.ico
$(top-build-dir)/Documentation/pictures/$(outbase)/lilypond.ico:
	$(MAKE) -C $(top-build-dir)/Documentation/pictures

$(outdir)/lilypond.ico: $(top-build-dir)/Documentation/pictures/$(outbase)/lilypond.ico
	cp $< $@
$(top-build-dir)/Documentation/pictures/$(outbase)/ly.ico:
	$(MAKE) -C $(top-build-dir)/Documentation/pictures

$(outdir)/ly.ico: $(top-build-dir)/Documentation/pictures/$(outbase)/ly.ico
	cp $< $@
endif

ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *(\([^,]+\)/\1/'
ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *([^"]*"\([^"]+\)"/\1/'

default:


$(outdir)/libstdc++.a:
	rm -f $@
	ln -s `$(CXX) -print-file-name=libstdc++.a` $(outdir)/

ifeq ($(LINK_GXX_STATICALLY),yes)
$(outdir)/lilypond: $(outdir)/libstdc++.a
endif

# force these: Make can't know these have to be generated in advance
$(outdir)/lily-parser.o: $(outdir)/parser.hh
$(outdir)/lily-lexer.o: $(outdir)/parser.hh $(outdir)/FlexLexer.h
$(outdir)/lexer.o: $(outdir)/parser.hh $(outdir)/version.hh

ifneq ($(FLEXLEXER_FILE),)
$(outdir)/FlexLexer.h: $(FLEXLEXER_FILE) $(config_h)
	cp $< $@
endif

$(outdir)/lexer.cc: $(config_h)
$(outdir)/parser.cc: $(config_h)

$(outdir)/general-scheme.o: $(outdir)/version.hh
$(outdir)/kpath.o: $(outdir)/version.hh
$(outdir)/lily-guile.o: $(outdir)/version.hh
$(outdir)/lily-version.o: $(outdir)/version.hh

local-po: $(outdir)/parser.cc

# ugh.  For --srcdir builds, these must exist to satisfy their broken
# lexer.dep and parser.dep file entries.
ifneq ($(configure-srcdir),.)
.PRECIOUS: $(outdir)/lexer.cc $(outdir)/parser.cc
endif
