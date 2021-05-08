\version "2.23.3"

\header {
  texidoc = "@file{turkish-makam.ly} sets accidental glyphs for all possible
grobs as well as standalone markups."

}

\include "accidental-glyphs-test.ily"
\include "turkish-makam.ly"
\test "accidentals.sharp.slashslash.stem" #1/12 ##t
