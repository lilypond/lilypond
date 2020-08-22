default: $(INFO_FILES)

ifeq ($(out),www)
local-WWW: $(OUT_TEXINFO_MANUALS) $(XREF_MAPS_FILES)
endif

local-txt-doc:  $(OUTTXT_FILES)
