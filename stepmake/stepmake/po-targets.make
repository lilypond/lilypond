XGETTEXT_OPTIONS = \
        --directory=$(src-dir) \
        --directory=. \
	--default-domain=$(package) \
	--from-code=UTF-8 \
	--join \
	--add-comments \
	--msgid-bugs-address="http://post.gmane.org/post.php?group=gmane.comp.gnu.lilypond.bugs" \
	--package-name=$(package) \
	--package-version=$(VERSION)

sed-header = \# Translation of LilyPond\n\# Copyright \(C\) 1998--2012 Han-Wen Nienhuys, Jan Nieuwenhuizen.\n\# This file is distributed under the same license as the LilyPond package.
sed-content = "Content-Type: text\/plain; charset=UTF-8\\n"

####
#### UGH!
new-po:
	if test -r $(po-srcdir); then \
	  rm -f $(po-outdir)/$(package).po; \
	  mkdir -p $(po-outdir); \
	  touch $(po-outdir)/$(package).po; \
	fi

ifeq ($(strip $(depth)),.)
po: new-po
	$(LOOP)
local-po:
	@true
else
po: local-po
	$(LOOP)
ALL_PO_SOURCES = $(ALL_C_SOURCES) $(ALL_CC_SOURCES) $(PYTHON_SCRIPTS_IN) $(PY_MODULES_IN) $(SCM_FILES)
local-po:
ifneq ($(strip $(ALL_PO_SOURCES)),)
	@echo $(ALL_PO_SOURCES)
	xgettext $(XGETTEXT_OPTIONS) --output-dir=$(po-outdir) \
	 --keyword=_ --keyword=_f --keyword=_i \
	 $(XGETTEXT_FLAGS) $(ALL_PO_SOURCES)
endif
	sed -i '1,2d' $(po-outdir)/$(package).po
	sed -i -e 's/^\# This file is distributed.*/$(sed-header)/' $(po-outdir)/$(package).po
	sed -i -e 's/^\"Content-Type: text\/plain.*/$(sed-content)/' $(po-outdir)/$(package).po
endif


po-update: po
	$(MAKE) -C $(po-outdir)/.. po-update

po-changes:
	$(MAKE) -C $(po-outdir)/.. po-changes

po-replace: po
	$(MAKE) -C $(po-outdir)/.. po-replace
