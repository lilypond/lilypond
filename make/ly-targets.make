convert-ly: local-convert-ly
	$(LOOP)

local-convert-ly:
	$(PYTHON) $(CONVERT_LY) --edit --assume-old *ly

local-help:
