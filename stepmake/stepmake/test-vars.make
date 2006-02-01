TEST_O_FILES := $(filter $(outdir)/test%, $(O_FILES))
O_FILES := $(filter-out $(outdir)/test%, $(O_FILES))

TEST_EXECUTABLE = $(outdir)/test-$(NAME)
TEST_LOADLIBES = $(LOADLIBES) -lboost_unit_test_framework
