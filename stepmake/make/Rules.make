# title	   package specific rules
# file	   make/Rules.lilymake.make

$(outdir)/%: %.in
	rm -f $@
	cat $< | $(sed-atfiles) | $(sed-atvariables) > $@


