# Documentation/topdocs/Makefile

depth = ../..

SUBDIRS=
BLURBS=BLURB #COPERTINA FLAPTEKST
AT_FILES = $(BLURBS) # 
at-dir = $(doc-dir)/
at-ext = .in

include $(depth)/make/Stepmake.make 


default: do-doc 

BLURBS=BLURB COPERTINA FLAPTEKST
# INFO_FILES = $(outdir)/$(package).info

include $(stepdir)/Documentation.shared.make

# urg
$(outdir)/%.html: %.yo
	$(YODL2HTML) -doutdir=$(outdir) --live-data=3 -o $@.in $<
	$(sed-version) < $@.in > $@
	rm -f $@.in
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) --index=index.html --news=$(depth)/NEWS $@

local-WWW:
	-cp $(outdir)/*gif $(outdir)/index.html $(depth)  # don't fail if not making website


TO_TOP_FILES=$(addprefix $(outdir)/, $(README_TOP_FILES))
copy-to-top: 
	$(foreach i, $(TO_TOP_FILES), \
	  cp $(i) $(depth)/ && ) true
