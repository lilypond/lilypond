.SUFFIXES: .c .dep .h .l .lo .o .so .y

$(outdir)/%.o: %.c
	$(call ly_progress,Making,$@,< c)
	$(DO_O_DEP) $(CC) -c $(ALL_CFLAGS) -o $@ $<

$(outdir)/%.o: $(outdir)/%.c
	$(call ly_progress,Making,$@,< c)
	$(DO_O_DEP) $(CC) -c $(ALL_CFLAGS) -o $@ $<

$(outdir)/%.lo: %.c
	$(call ly_progress,Making,$@,< c)
	$(DO_LO_DEP) $(CC) -c $(ALL_CFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.lo: %.c
	$(call ly_progress,Making,$@,< c)
	$(DO_LO_DEP) $(CC) -c $(ALL_CFLAGS) $(PIC_FLAGS) -o $@ $<

$(outdir)/%.c $(outdir)/%.h: %.y
	$(call ly_progress,Making,$@,< y)
	$(BISON) -d -o $(outdir)/$*.c $<

$(outdir)/%.c: %.l
	$(call ly_progress,Making,$@,< l)
	$(FLEX) -Cfe -p -p -o$@ $<
# could be faster:
#	$(FLEX) -8 -Cf -o$@ $<

$(outdir)/%.rc.o: $(outdir)/%.rc
	$(call ly_progress,Making,$@,< rc)
	$(WINDRES) $(WINDRES_FLAGS) -o$@ $<
