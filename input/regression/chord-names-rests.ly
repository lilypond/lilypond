\version "2.23.10"

\header {
  texidoc = "In @code{ChordNames}, both normal rests and multi-measure rests
cause @code{noChordSymbol} to be printed.  Skips do not print anything."
}

mus = \chordmode { r1 s1 R1 }

<<
  \new ChordNames \mus
  \mus
>>
