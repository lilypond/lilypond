\version "2.11.51"

\header {
  texidoc = "RehearsalMarks still align correctly if Mark_engraver is moved to
another context."
}

\layout {
  \context {
    \Staff
    \consists "Mark_engraver"
  }
  \context {
    \Score
    \remove "Mark_engraver"
  }
}

\relative {
  c1 \mark "foo"
  c1
  \key cis \major
  \clef alto
  \override Score.RehearsalMark #'break-align-symbols = #'(key-signature)
  \mark "on-key"
  cis
  \key ces \major
  \override Score.RehearsalMark #'break-align-symbols = #'(clef)
  \clef treble
  \mark "on clef"
  ces
}
