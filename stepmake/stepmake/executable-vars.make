# empty
LDFLAGS = $(ILDFLAGS) $(EXTRA_LDFLAGS) $(MODULE_LDFLAGS) $($(PACKAGE)_LDFLAGS)

## urg, silly name: library.a
MODULE_LIBES =$(addprefix $(outdir)/../, $(addsuffix /$(outbase)/library.a, $(MODULE_LIBS)))
LOADLIBES = $(MODULE_LIBES) $($(PACKAGE)_LIBES) $(EXTRA_LIBES)
