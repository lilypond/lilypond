#(ly:set-option 'old-relative)
\version "1.9.0"

\header {
  texidoc = "Melody and lyrics."
}

melody = \notes \relative c'' {
  a b c d
}

text = \lyrics {
  Aaa Bee Cee Dee
}

\score {
  <
    \addlyrics
      \context Staff = one {
        \property Staff.autoBeaming = ##f
        \melody
      }
      \context Lyrics \text
  >
  \paper { }
  \midi  { }
}
