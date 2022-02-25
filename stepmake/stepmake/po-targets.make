ALL_PO_SOURCES = \
  $(ALL_CC_SOURCES) \
  $(PYTHON_SCRIPTS_IN) \
  $(PY_MODULES_IN) \
  $(SCM_FILES)

XGETTEXT_OPTIONS = \
  --directory=$(src-dir) \
  --directory=. \
  --default-domain=$(package) \
  --from-code=UTF-8 \
  --join \
  --add-comments \
  --msgid-bugs-address="https://mail.gnu.org/mailman/listinfo/bug-lilypond" \
  --package-name=$(package) \
  --package-version=$(VERSION)

sed-header = \# Translation of LilyPond\n\# Copyright \(C\) 1998--2022 Han-Wen Nienhuys, Jan Nieuwenhuizen.\n\# This file is distributed under the same license as the LilyPond package.
sed-content = "Content-Type: text\/plain; charset=UTF-8\\n"

po: local-po
	$(LOOP)

local-po:
	$(call ly_progress,Making po files for,*,)
	xgettext $(XGETTEXT_OPTIONS) --output-dir=$(po-outdir) \
	  --keyword=_ --keyword=_f --keyword=_i --keyword=G_ \
	  $(XGETTEXT_FLAGS) $(ALL_PO_SOURCES)
	sed -i '1,2d' $(po-outdir)/$(package).po
	sed -i -e 's/^\# This file is distributed.*/$(sed-header)/' $(po-outdir)/$(package).po
	sed -i -e 's/^\"Content-Type: text\/plain.*/$(sed-content)/' $(po-outdir)/$(package).po
