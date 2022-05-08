depth = ..

NAME = flower

# test uses LILYPOND_DATADIR
TEMPLATES= c++ po lilypond
export top-src-dir

include $(depth)/make/lilypond.make

TEST_O_FILES := $(filter $(outdir)/test%, $(O_FILES))
O_FILES := $(filter-out $(outdir)/test%, $(O_FILES))

TEST_EXECUTABLE = $(outdir)/test-$(NAME)
TEST_LOADLIBES = $(LIBRARY) $(CXXABI_LIBS)

# yaffut.hh catches all exceptions, so re-enable -fexceptions for the tests.
$(TEST_O_FILES): EXTRA_CXXFLAGS += -fexceptions

$(TEST_EXECUTABLE): $(TEST_O_FILES) $(O_FILES)
	$(call ly_progress,Making,$@,)
	$(CXX) -o $@ $(TEST_O_FILES) $(TEST_LOADLIBES) $(ALL_LDFLAGS)

.PHONY: check test

check: test

test: $(TEST_EXECUTABLE)
	$(call ly_progress,Running,$(TEST_EXECUTABLE),)
	$(TEST_EXECUTABLE) $(if $(VERBOSE),,--quiet)

AR=ar
LIBRARY = $(outdir)/library.a

$(O_FILES): $(outdir)/config.hh

$(LIBRARY): $(O_FILES)
	$(call ly_progress,Making,$@,)
	$(RM) $@
	$(AR) rc $@ $(O_FILES)
	$(RANLIB) $@ || $(AR) ts $@ || true

default: $(LIBRARY)
