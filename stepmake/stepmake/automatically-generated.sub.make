
LINECOMMENT=\#

default:
	-chmod +w $(OUTFILE)
	head -1 $(INFILE) > $(OUTFILE)
	echo "$(LINECOMMENT) WARNING WARNING WARNING" >> $(OUTFILE)
	echo "$(LINECOMMENT) do not edit! this is $(OUTFILE), generated from $(INFILE)" >> $(OUTFILE)
	cat $(INFILE) >> $(OUTFILE)
	chmod -w $(OUTFILE)
