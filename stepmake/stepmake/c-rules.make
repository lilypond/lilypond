.SUFFIXES: .c .dep .h .l .lo .o .so .y

$(outdir)/%.o: %.c
	$(DO_O_DEP) $(CC) -c $(CFLAGS) -o $@ $<

$(outdir)/%.o: $(outdir)/%.c
	$(DO_O_DEP) $(CC) -c $(CFLAGS) -o $@ $<

$(outdir)/%.lo: %.c
	$(DO_LO_DEP) $(CC) -c $(CFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.lo: %.c
	$(DO_LO_DEP) $(CC) -c $(CFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.c: %.y
	$(BISON) $<
#	mv $<.tab.c $@
	mv parser.tab.c $@

$(outdir)/%.h: %.y
	$(BISON) -d $<
#	mv $<.tab.h $@
	mv parser.tab.h $@
	mv parser.tab.c $(basename $@).c

$(outdir)/%.c: %.l
	$(FLEX) -Cfe -p -p -t $< > $@
# could be faster:
#	$(FLEX) -8 -Cf -t $< > $@

