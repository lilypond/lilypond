
$(outdir)/%.dvi: %.mf
	mf $<
	gftodvi  $(basename $<)
#	mv $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.log: %.mf
	mf $<
#	mv $(@F) $@
	rm $(basename $< ).*gf

$(lyout)/%.ly $(texout)/%.tex: $(outdir)/%.log
	$(PYTHON) $(depth)/bin/mf-to-table --ly $(lyout)/$(<F:.log=.ly) --tex $(texout)/$(<F:.log=.tex) $<

$(MFDEPS): $(FONT_FILES)
# do something silly to avoid barfs if python not installed.
	echo > $@
	$(PYTHON) $(depth)/bin/mf-deps $^ >> $@

# silly workaround for stupid TeXs
systempks:
# irix 5.3
#	MakeTeXPK feta16 300 300 magstep\(0.0\)
	set -e ; for a in $(FONT_FILES); do \
	MakeTeXPK `basename $$a .mf` 300 300 magstep\(0.0\) ; \
	mf "\mode=ljfour; input `basename $$a .mf`"  ;\
#	mv -f `find . -name '*.tfm' -print -o -name '*gf' -print`  out/ ; \
	mv -f `find . -name '*.tfm' -print -o -name '*gf' -print` ; \
	done

