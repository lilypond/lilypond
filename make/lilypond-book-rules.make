.SUFFIXES: .html .xml .lytex .tex .latex

$(outdir)/%.html:  %.html
	../../../out/bin/lilypond-book -o $(outdir) $<

$(outdir)/%.html:  %.xml
	../../../out/bin/lilypond-book -o $(outdir) $<

$(outdir)/%.tex:  %.lytex
	../../../out/bin/lilypond-book --pdf -o $(outdir) $<

$(outdir)/%.tex:  %.tex
	../../../out/bin/lilypond-book --pdf -o $(outdir) $<

$(outdir)/%.tex:  %.latex
	../../../out/bin/lilypond-book --pdf -o $(outdir) $<

$(outdir)/%.pdf:  $(outdir)/%.tex
	cd $(outdir) && pdflatex $(notdir $<)
