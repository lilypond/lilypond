
$(top_builddir)/mf/%.dvi: %.mf
	mf $<
	gftodvi  $(basename $<)
#	mv $(basename $<).dvi $(outdir)
#	rm $(basename $<).*gf

$(top_builddir)/mf/%.log: %.mf
	mf $<
#	mv $(@F) $@
#	rm $(basename $< ).*gf

$(lyout)/%.ly $(texout)/%.tex: $(top_builddir)/mf/%.log
	$(PYTHON) $(top_builddir)/bin/mf-to-table --ly $(lyout)/$(<F:.log=.ly) --tex $(texout)/$(<F:.log=.tex) $<

$(MFDEPS): $(FONT_FILES)
# do something silly to avoid barfs if python not installed.
	echo > $@
	$(PYTHON) $(top_builddir)/bin/mf-deps $^ >> $@

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

