# stepmake/C_rules.make

.SUFFIXES: .c .o .h .y .l .dep

$(outdir)/%.o: %.c
	$(DO_C_COMPILE)

$(outdir)/%.o: $(outdir)/%.c
	$(DO_C_COMPILE)

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

