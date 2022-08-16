
depth = ..

NAME = lilypond

MODULE_LIBS=$(depth)/flower
MODULE_INCLUDES=$(src-depth)/flower/include

HELP2MAN_EXECS = lilypond
TEMPLATES = c++ po help2man

SUBDIRS = include

include $(depth)/make/lilypond.make

FLOWER_LIB = $(depth)/flower/$(outdir)/library.a
LDLIBS = $(FLOWER_LIB) $(CONFIG_LIBS)

EXECUTABLE = $(outdir)/$(NAME)$(program_suffix)

default: $(EXECUTABLE)

local-install: installexe

local-uninstall: uninstallexe

installexe: all
	-$(INSTALLPY) -d $(DESTDIR)$(bindir)
	$(INSTALLPY) -m 755 $(EXECUTABLE) $(DESTDIR)$(bindir)/$(program_prefix)$(notdir $(EXECUTABLE))

uninstallexe:
	rm -f $(DESTDIR)$(bindir)/$(program_prefix)$(notdir $(EXECUTABLE))

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
$(EXECUTABLE): $(outdir)/lilypond.rc.o

$(outdir)/lilypond.rc.o: $(outdir)/lilypond.ico $(outdir)/ly.ico \
	$(src-dir)/lilypond.manifest

$(top-build-dir)/Documentation/$(outbase)/logo/%.ico:
	$(MAKE) -C $(top-build-dir)/Documentation $(outbase)/logo/$*.ico

$(outdir)/%.ico: $(top-build-dir)/Documentation/$(outbase)/logo/%.ico
	$(call ly_progress,Making,$@,(copy))
	cp $< $@
endif

$(outdir)/libstdc++.a:
	$(call ly_progress,Making,$@,(symlink))
	rm -f $@
	ln -s `$(CXX) -print-file-name=libstdc++.a` $(outdir)

ifeq ($(LINK_GXX_STATICALLY),yes)
$(EXECUTABLE): $(outdir)/libstdc++.a
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

pre := {c++}/[ \t]*
second := [ \t\n\v]*([^,]*,[^""]*\"\([^""]*\)/\1/m
third  := [ \t\n\v]*([^,]*,[^,]*,[^""]*\"\([^""]*\)/\1/m
three-arg-calls := MAKE_SCHEME_CALLBACK_WITH_OPTARGS \
                   MAKE_DOCUMENTED_SCHEME_CALLBACK \
                   MAKE_SCHEME_CALLBACK
two-arg-calls := LY_DEFINE \
                 LY_DEFINE_WITH_SETTER

regex_ =
regex_ += $(foreach fname,$(two-arg-calls),--regex='$(pre)$(fname)$(second)')
regex_ += $(foreach fname,$(three-arg-calls),--regex='$(pre)$(fname)$(third)')

ETAGS_FLAGS += $(regex_)
CTAGS_FLAGS += $(regex_)

ifeq ($(strip $(CROSS)),no)
$(outdir)/%.1: $(outdir)/%$(program_suffix) $(buildscript-dir)/help2man.pl
	$(call ly_progress,Making,$@,(help2man))
	LANGUAGE= $(PERL) $(buildscript-dir)/help2man.pl $< > $@
endif
