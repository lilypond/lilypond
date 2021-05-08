\version "2.23.3"

\header {
  texidoc = "@file{arabic.ly} sets accidental glyphs for all possible
grobs as well as standalone markups."
}

\include "accidental-glyphs-test.ily"
\include "arabic.ly"
\test "accidentals.flat.slash" #-1/4 ##t
