\version "2.23.14"

\header {
  texidoc = "Marks still align correctly if Mark_engraver is moved to
Staff context."
}

\layout {
  \context {
    \Staff
    \consists Text_mark_engraver
  }
  \context {
    \Score
    \remove Text_mark_engraver
  }
}

\relative {
  c'1 \textMark "foo"
  c1
  \key cis \major
  \clef alto
  \override Score.TextMark.break-align-symbols = #'(key-signature)
  \textMark "on-key"
  cis
  \key ces \major
  \override Score.TextMark.break-align-symbols = #'(clef)
  \clef treble
  \textMark "on clef"
  ces
}
