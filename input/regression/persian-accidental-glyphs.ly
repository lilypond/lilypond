\version "2.23.6"

\header {
  texidoc = "One notation style for Persian music uses the @emph{sori} and
@emph{koron} accidental glyphs."
}

VaziriGlyphs = #'(
  (0 . "accidentals.natural")
  (-1/2 . "accidentals.flat")
  (1/2 . "accidentals.sharp")

  (1/4 . "accidentals.sharp.sori")
  (-1/4 . "accidentals.flat.koron")
)

\paper {
  font-defaults.alteration-glyph-name-alist = \VaziriGlyphs
}

\relative c' {
  c!4 cih cis deh |
  g! gih gis aeh |
  d! deh des cih |
  a'! aeh aes gih }
