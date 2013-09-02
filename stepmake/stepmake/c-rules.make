.SUFFIXES: .c .dep .h .l .lo .o .so .y

$(outdir)/%.o: %.c
	$(DO_O_DEP) $(CC) -c $(ALL_CFLAGS) -o $@ $<

$(outdir)/%.o: $(outdir)/%.c
	$(DO_O_DEP) $(CC) -c $(ALL_CFLAGS) -o $@ $<

$(outdir)/%.lo: %.c
	$(DO_LO_DEP) $(CC) -c $(ALL_CFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.lo: %.c
	$(DO_LO_DEP) $(CC) -c $(ALL_CFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.c $(outdir)/%.h: %.y
	$(BISON) -d -o $(outdir)/$*.c $<

$(outdir)/%.c: %.l
	$(FLEX) -Cfe -p -p -o$@ $<
# could be faster:
#	$(FLEX) -8 -Cf -o$@ $<

$(outdir)/%.rc.o: $(outdir)/%.rc
	$(WINDRES) $(WINDRES_FLAGS) -o$@ $<
