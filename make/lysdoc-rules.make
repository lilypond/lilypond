# We can't print the list in one go, as it might be longer than a
# shell command is allowed (as of linux 2.6.3x >131000 chars)
# Split it up into 10 300-element chunks, and one chunk containing the rest
# if we have more than 3000 elements.
$(outdir)/collated-files.list: $(COLLATED_FILES)
	$(call ly_progress,Making,$@,< $(words $^) files)
	echo $(wordlist    1, 299,$^)>$@
	echo $(wordlist  300, 599,$^)>>$@
	echo $(wordlist  600, 899,$^)>>$@
	echo $(wordlist  900,1199,$^)>>$@
	echo $(wordlist 1200,1499,$^)>>$@
	echo $(wordlist 1500,1799,$^)>>$@
	echo $(wordlist 1800,2099,$^)>>$@
	echo $(wordlist 2100,2399,$^)>>$@
	echo $(wordlist 2400,2699,$^)>>$@
	echo $(wordlist 2700,2999,$^)>>$@
	echo $(wordlist 3000,$(words $^),$^)>>$@

$(outdir)/collated-files.tely: $(outdir)/collated-files.list
	$(call ly_progress,Making,$@,)
	$(LYS_TO_TELY) --output=$(outdir)/collated-files.tely \
	               --name=collated-files.info \
	               --title="$(TITLE)" \
	               --author="$(AUTHOR)" \
	               --input-filename=$^

# This tells make that $(COLLATED_FILES) must exist in order to build
# this target, i.e. they can't be treated as intermediates.
$(outdir)/collated-files.texi: $(COLLATED_FILES)
