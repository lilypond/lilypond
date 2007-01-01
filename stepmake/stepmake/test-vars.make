TEST_O_FILES := $(filter $(outdir)/test%, $(O_FILES))
O_FILES := $(filter-out $(outdir)/test%, $(O_FILES))

TEST_EXECUTABLE = $(outdir)/test-$(NAME)
TEST_MODULE_LIBES =$(addprefix $(outdir)/../, $(addsuffix /$(outbase)/library.a, $(TEST_MODULE_LIBS)))

TEST_LOADLIBES = $(TEST_MODULE_LIBES) $(LOADLIBES)
