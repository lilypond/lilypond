
default: local-txt-doc

ifeq ($(out),www)
local-WWW-1: $(PDF_FILES)

local-WWW-2: $(HTML_FILES)
endif

txt-files: $(TO_TOP_FILES)
