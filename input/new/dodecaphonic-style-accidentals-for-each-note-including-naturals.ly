\version "2.12.0"

\header {
  lsrtags = "pitches"
  texidoc = "In early 20th century works, starting with Schoenberg,
Berg and Webern (the \"Second\" Viennese school), every pitch in the
twelve-tone scale has to be regarded as equal, without any hierarchy
such as the classical (tonal) degrees.  Therefore, these composers
print one accidental for each note, even at natural pitches, to
emphasize their new approach to music theory and language.

This snippet shows how to achieve such notation rules. 
"

  doctitle = "Dodecaphonic-style accidentals for each note including naturals"
}

\score {
  \new Staff {
    #(set-accidental-style 'dodecaphonic)
    c'4 dis' cis' cis'
    c'4 dis' cis' cis'
    c'4 c' dis' des'
  }
  \layout {
    \context {
      \Staff
      \remove "Key_engraver"
    }
  }
}
