
default: local-doc

local-WWW: $(HTML_FILES) $(PDF_FILES) $(TO_TOP_FILES)

make-txt-files: $(addprefix $(outdir)/,$(addsuffix .txt,$(TO_TOP_FILES)))
