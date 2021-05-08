\version "2.23.3"

\header {
  texidoc = "The @code{alterationGlyphs} context property maps
alterations (as rationals) to glyph names.  It is used by
all grobs that print accidentals."
}

\include "accidental-glyphs-test.ily"

\layout {
  \context {
    \Score
    alterationGlyphs =
      #`((,SHARP . "accidentals.sharp.slashslashslash.stem"))
  }
}

\test "accidentals.sharp.slashslashslash.stem" #1/2 ##f
