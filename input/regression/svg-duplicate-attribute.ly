\version "2.25.1"

\header {
  texidoc = "SVG output attributes are not duplicated.

The proper way to know if this test passes is to compile
it into an SVG file (with @option{--svg}) and check the
generated SVG code."
}

{
  \once \override NoteHead.output-attributes.valI = "x"
  \once \override NoteHead.output-attributes.valII = "y"
  \once \override NoteHead.output-attributes.valIII = "z"
  \once \override NoteHead.output-attributes.valII = "t"
  \once \override NoteHead.output-attributes.valIV = "u"
  c'
}
