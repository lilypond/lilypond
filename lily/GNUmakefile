
depth = ..

NAME = lilypond

MODULE_LIBS=$(depth)/flower
MODULE_INCLUDES=$(src-depth)/flower/include

HELP2MAN_EXECS = lilypond
STEPMAKE_TEMPLATES=c++ po help2man

include $(depth)/make/stepmake.make

FLOWER_LIB = $(depth)/flower/$(outdir)/library.a
LDLIBS = $(FLOWER_LIB) $(CONFIG_LIBS)

EXECUTABLE = $(outdir)/$(NAME)

default: $(EXECUTABLE)

local-install: installexe

local-uninstall: uninstallexe

installexe: all
	-$(INSTALLPY) -d $(DESTDIR)$(bindir)
	$(INSTALLPY) -m 755 $(EXECUTABLE) $(DESTDIR)$(bindir)/$(program_prefix)$(notdir $(EXECUTABLE))$(program_suffix)

uninstallexe:
	rm -f $(DESTDIR)$(bindir)/$(program_prefix)$(notdir $(EXECUTABLE))$(program_suffix)

$(O_FILES): $(outdir)/config.hh

$(FLOWER_LIB):
	$(MAKE) -C $(dirname $@)

$(EXECUTABLE): $(O_FILES) $(outdir)/version.hh $(FLOWER_LIB)
	$(call ly_progress,Making,$@,)
	$(foreach a, $(MODULE_LIBS), $(MAKE) -C $(a) && ) true
	$(CXX) $(ALL_CXXFLAGS) -o $@ $(O_FILES) $(LDLIBS) $(ALL_LDFLAGS)


ifeq ($(GS_API),yes)
MODULE_LDFLAGS += -lgs
endif
ifeq ($(LINK_GXX_STATICALLY),yes)
MODULE_LDFLAGS += -L$(outdir) -static-libgcc
endif

# for profiling, link guile statically:
#
# CONFIG_LDFLAGS += -static -lltdl -ldl
#

ifeq ($(PLATFORM_WINDOWS),yes)
WINDRES_FLAGS += -DLilyPond=0 -DLY=1
O_FILES += $(outdir)/lilypond.rc.o
$(outdir)/lilypond: $(outdir)/lilypond.rc.o

$(outdir)/lilypond.rc.o: $(outdir)/lilypond.ico $(outdir)/ly.ico \
	$(src-dir)/lilypond.manifest

$(top-build-dir)/Documentation/$(outbase)/logo/%.ico:
	$(MAKE) -C $(top-build-dir)/Documentation $(outbase)/logo/$*.ico

$(outdir)/%.ico: $(top-build-dir)/Documentation/$(outbase)/logo/%.ico
	$(call ly_progress,Making,$@,(copy))
	cp $< $@
endif

ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *(\([^,]+\)/\1/'
ETAGS_FLAGS += --regex='{c++}/^LY_DEFINE *([^"]*"\([^"]+\)"/\1/'

$(outdir)/libstdc++.a:
	$(call ly_progress,Making,$@,(symlink))
	rm -f $@
	ln -s `$(CXX) -print-file-name=libstdc++.a` $(outdir)

ifeq ($(LINK_GXX_STATICALLY),yes)
$(outdir)/lilypond: $(outdir)/libstdc++.a
endif

# force these: Make can't know these have to be generated in advance
$(outdir)/lily-parser.o $(outdir)/parser.o: $(outdir)/parser.hh
$(outdir)/lily-lexer.o: $(outdir)/parser.hh $(config_h)
$(outdir)/lexer.o: $(outdir)/parser.hh $(outdir)/version.hh
$(outdir)/lily-lexer-scheme.o \
  $(outdir)/lily-parser-scheme.o \
  $(outdir)/parse-scm.o \
  $(outdir)/sources.o: $(outdir)/parser.hh

$(outdir)/lexer.cc: $(config_h)
$(outdir)/parser.cc: $(config_h)

ifneq ($(LILYPOND_EXTERNAL_BINARY),)
$(outdir)/lilypond.1: $(LILYPOND_EXTERNAL_BINARY) \
                      $(buildscript-dir)/help2man.pl
	$(call ly_progress,Making,$@,(help2man))
	LANGUAGE= $(PERL) $(buildscript-dir)/help2man.pl \
                            $(LILYPOND_EXTERNAL_BINARY) > $@
endif

$(outdir)/general-scheme.o: $(outdir)/version.hh
$(outdir)/lily-version.o: $(outdir)/version.hh
$(outdir)/relocate.o: $(outdir)/version.hh

local-po: $(outdir)/parser.cc
