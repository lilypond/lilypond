
.SUFFIXES: .po .pot .mo

$(outdir)/%.mo: %.po
	$(MSGFMT) -o $@ $<

# sed-pofile = sed 's/^. \#: .*//'
sed-pofile = -e 's/^\#: .*//'
sed-makestuff = -e 's/[a-zA-Z_/]*make\[[0-9]*\].*//'
sed-edstuff = -e 's/[ \.,adic0-9]*//' -e 's/---//' | sort -u


po-update:
	$(foreach i,$(CATALOGS), \
	  rm -f $(po-outdir)/$(i).po; \
	  msgmerge $(po-srcdir)/$(i).po $(po-outdir)/$(package).po \
	    -o $(po-outdir)/$(i).po && ) true
	@$(foreach i,$(CATALOGS), \
	  changes=`$(MAKE) --silent -C $(po-outdir)/.. LANGUAGE=$i po-changes $(ERROR_LOG) | sed $(sed-makestuff)`; \
	  if test "$$changes" != "" ; then \
	    echo "*** Changes for language $i; check $(po-outdir)/$i.po ***"; \
	    echo -e "changes: \`$$changes'";\
	  fi && ) true


po-changes:
	diff -e $(po-outdir)/$(LANGUAGE).po $(po-srcdir)/$(LANGUAGE).po \
	  | sed $(sed-pofile) $(sed-edstuff)

po-replace: po-update
	mv $(outdir)/$(package).po $(package).pot
