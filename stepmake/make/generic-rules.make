# title	   package specific rules

#UGH

include $(depth)/make/substitute.make

$(outdir)/%: %.in
	rm -f $@
	cat $< | $(sed-atfiles) | $(sed-atvariables) > $@


