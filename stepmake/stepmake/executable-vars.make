MODULE_LIBES =$(addprefix $(outdir)/../, $(addsuffix /$(outbase)/library.a, $(MODULE_LIBS)))
LOADLIBES = $(MODULE_LIBES) $($(PACKAGE)_LIBES) $(EXTRA_LIBES)

EXECUTABLE = $(outdir)/$(NAME)
EXECUTABLES = $(notdir $(EXECUTABLE))


