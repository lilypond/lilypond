\version "1.5.68"

\header {
  texidoc = "lead sheet format: chords with melody."
}

harmonies = \chords {
  c4:m f:min7 g:maj c:aug d2:dim b:sus
}

melody = \notes \relative c' {
  f4 [e8 c] d4 g | a2 ~ a2
}

\score {
  \notes <
    \context ChordNames {
        \property ChordNames.chordChanges = ##t
        \harmonies
    }
    \context Staff = one \melody
  >

  \paper{ }
  \midi{ }
}
