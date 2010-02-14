.SUFFIXES: .html .xml

$(outdir)/%.html:  %.html
	../../../out/bin/lilypond-book -o $(outdir) $<

$(outdir)/%.html:  %.xml
	../../../out/bin/lilypond-book -o $(outdir) $<
