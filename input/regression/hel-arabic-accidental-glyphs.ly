\version "2.23.3"

\header {
  texidoc = "@file{hel-arabic.ly} sets accidental glyphs for all possible
grobs as well as standalone markups."

}

\include "accidental-glyphs-test.ily"
\include "hel-arabic.ly"
\test "accidentals.flatflat.slash" #-5/2 ##t
