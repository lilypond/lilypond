# Texinfo_rules.make

.SUFFIXES: .1 .html .texinfo

texi2man = $(step-bindir)/out/texi2man $< $@
texi2html =\
    rm -f $@.urg;\
    cat $< | sed "s!@include.*!!" > $@.urg;\
    $(TEXI2HTML) $@.urg;\
    rm -f $@.urg;\
    mv $(@F).urg.html $@;\
    rm -f $(@F:%.html=%.html.urg_toc.html);\
    add-URLs $@

$(outdir)/%.1: %.texinfo
	$(texi2man)

$(outdir)/%.info: $(outdir)/%.texinfo
	$(MAKEINFO) --force -o $@ $<

# $(outdir)/%.html: %.texinfo
# 	$(texi2html)
# 	add-html-footer --index $(depth)/../index.html $@

# texi2html is as broken as pod2html 5004 :-) 
# we'll have to use pod2html 5003 for now.
# don't burn your .pod sources!
#
$(outdir)/%.html: $(outdir)/%.texinfo
	$(texi2html)
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) --index $(depth)/../index.html $@

$(outdir)/%.1: $(outdir)/%.texinfo
	-$(texi2man)

