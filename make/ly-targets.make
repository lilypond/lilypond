# empty

convert-ly: local-convert-ly
	$(LOOP)

local-convert-ly:
	$(PYTHON) $(CONVERT_LY) --edit --assume-old *ly

local-help: local-ly-help

local-ly-help:
	@echo -e "\
  convert-ly  convert all LilyPond sources\n\
"\
#


