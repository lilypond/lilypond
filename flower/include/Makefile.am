# Generated automatically by wild-perl 0.1
# flower/include/Makefile.am.wild

noinst_HEADERS = acursor.hh arithmetic-operator.hh assoc-iter.hh assoc.hh choleski.hh compare.hh cpu-timer.hh cursor.hh data-file.hh diagonal-storage.hh dictionary-iter.hh dictionary.hh directed-graph.hh dstream.hh flower-debug.hh fproto.hh full-storage.hh handle.hh international.hh interval.hh iterate.hh lgetopt.hh libc-extension.hh link.hh list.hh matrix-storage.hh matrix.hh nscalar.hh parray.hh path.hh pcursor.hh plist.hh pointer.hh pqueue.hh priorities.hh rational.hh real.hh scalar.hh string-convert.hh string-data.hh string-handle.hh string.hh text-db.hh text-stream.hh timer.hh unionfind.hh varray.hh vector.hh virtual-methods.hh cursor.icc full-storage.icc link.icc list.icc plist.icc string-data.icc string-handle.icc string.icc cursor.tcc interval.tcc list.tcc pcursor.tcc plist.tcc pointer.tcc

EXTRA_DIST = Makefile.am.wild GNUmakefile

# override default target for all-am: to get an extra dependency
# all-am: Makefile $(PROGRAMS) wild-check
# ugh, automake doesn't generate the all-am target here...
all: Makefile $(HEADERS) wild-check

# dummy target for non-GNU makes; is overridden in GNUmakefile
wild-check:

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@