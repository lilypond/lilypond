\version "2.23.3"

\header {
  texidoc = "@file{makam.ly} sets accidental glyphs for all possible
grobs as well as standalone markups."

}

\include "accidental-glyphs-test.ily"
\include "makam.ly"
\test "accidentals.sharp.slashslashslash.stemstem" #8/9 ##t
