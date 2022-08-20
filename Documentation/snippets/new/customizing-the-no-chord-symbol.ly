\version "2.23.10"

\header {
  lsrtags = "chords"

  texidoc = "
By default, rests in a @code{ChordNames} context cause the @qq{N.C.}
symbol to be printed.  This markup can be customized.
"

  doctitle = "Customizing the no-chord symbol"
}


<<
  \chords {
    R1
    \set noChordSymbol = "—"
    R1
    \set noChordSymbol = \markup \italic "Ssh!"
    R1
  }
  {
    R1*3
  }
>>
