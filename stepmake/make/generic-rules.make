# title	   package specific rules
# file	   make/Rules.lilymake.make

#UGH

include $(depth)/make/substitute.make

$(outdir)/%: %.in
	rm -f $@
	cat $< | $(sed-atfiles) | $(sed-atvariables) > $@


