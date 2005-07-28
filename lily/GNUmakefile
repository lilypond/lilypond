
depth = ..

NAME = lilypond
SUBDIRS = include

MODULE_LIBS=  $(depth)/flower   $(depth)/kpath-guile 
MODULE_INCLUDES= $(depth)/flower/include 
MODULE_CXXFLAGS=

HELP2MAN_EXECS = lilypond
STEPMAKE_TEMPLATES=c c++ executable po help2man

OUT_DIST_FILES=$(addprefix $(outdir)/,parser.cc parser.hh )

include $(depth)/make/stepmake.make 

ifeq ($(HAVE_LIBKPATHSEA_SO),no)
MODULE_LDFLAGS+= $(KPATHSEA_LIBS)
endif

ifeq ($(LINK_GXX_STATICALLY),yes)
MODULE_LDFLAGS += -L$(outdir) -static-libgcc
endif


# for profiling, link guile statically:
#
# USER_LDFLAGS += -static -lltdl -ldl
#

ifeq ($(PLATFORM_WINDOWS),yes)
WINDRES_FLAGS += -DLilyPond=0 -DLY=1
O_FILES += $(outdir)/lilypond.rc.o
$(outdir)/lilypond: $(outdir)/lilypond.rc.o

$(outdir)/lilypond.rc.o: $(outdir)/lilypond.ico $(outdir)/ly.ico
$(builddir)/Documentation/pictures/$(outbase)/lilypond.ico:
	$(MAKE) -C $(abs-srcdir)/Documentation/pictures
$(outdir)/lilypond.ico: $(builddir)/Documentation/pictures/$(outbase)/lilypond.ico
	cp $< $@ 
$(builddir)/Documentation/pictures/$(outbase)/ly.ico:
	$(MAKE) -C $(abs-srcdir)/Documentation/pictures
$(outdir)/ly.ico: $(builddir)/Documentation/pictures/$(outbase)/ly.ico
	cp $< $@ 
endif

ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *(\([^,]+\)/\1/'
ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *([^"]*"\([^"]+\)"/\1/'

default:


$(outdir)/libstdc++.a: 
	ln -s `$(CXX) -print-file-name=libstdc++.a` $(outdir)/

ifeq ($(LINK_GXX_STATICALLY),yes)
$(outdir)/lilypond: $(outdir)/libstdc++.a
endif


# force these: Make can't know these have to be generated in advance
$(outdir)/lily-parser.o: $(outdir)/parser.hh
$(outdir)/lily-lexer.o: $(outdir)/parser.hh $(outdir)/FlexLexer.h
$(outdir)/lexer.o: $(outdir)/parser.hh $(outdir)/version.hh

ifneq ($(FLEXLEXER_PATH),)
$(outdir)/FlexLexer.h: $(FLEXLEXER_PATH)/FlexLexer.h
	cp $<  $@
endif



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

