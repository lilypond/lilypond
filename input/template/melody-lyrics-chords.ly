\version "1.5.68"

\header {
  texidoc = "popsong format: chords, melody and lyrics."
}

melody = \notes \relative c' {
  a b c d
}

text = \lyrics {
  Aaa Bee Cee Dee
}

accompaniment = \chords {
  a2 c2
}

\score {
  <
    \context ChordNames \accompaniment
    \addlyrics
      \context Staff = one {
        \property Staff.autoBeaming = ##f
        \property Staff.automaticMelismata = ##t
        \melody
      }
      \context Lyrics \text
  >
  \paper { }
  \midi  { }
}
