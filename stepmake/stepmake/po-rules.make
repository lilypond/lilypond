# Porules.make

.SUFFIXES: .po .mo

$(outdir)/%.mo: %.po
	$(MSGFMT) -o $@ $<

# sed-pofile = sed 's/^. \#: .*//'
sed-pofile = sed 's/^\#: .*//'
sed-makestuff = sed 's/[a-zA-Z_/]*make\[[0-9]*\].*//'
sed-edstuff = sed 's/[ \.,adic0-9]*//' | sed 's/---//' | sort -u

po-update:
	$(foreach i,$(CATALOGS), \
	  rm -f $(po-dir)/$(outdir)/$(i).po; \
	  tupdate $(po-dir)/$(outdir)/$(package).po $(po-dir)/$(i).po \
	    > $(po-dir)/$(outdir)/$(i).po && ) true
	$(foreach i,$(CATALOGS), \
	  changes=`$(MAKE) --silent -C $(po-dir) LANGUAGE=$$i show-po-changes $(ERROR_LOG) | $(sed-makestuff)`; \
	  if test "$$changes" != "" ; then \
	    echo "*** Changes for language $$i; check po/$(outdir)/$$i.po ***"; \
	    echo -e "changes: \`$$changes'";\
	  fi; && ) true


show-po-changes:
	diff -e $(po-dir)/$(outdir)/$(LANGUAGE).po $(po-dir)/$(LANGUAGE).po \
	  | $(sed-pofile) | $(sed-edstuff)
