\version "2.15.12"

\header {
  lsrtags = "expressive marks, staff-notation, tweaks-and-overrides"
  texidoc = "@code{NoteColumn} grobs can be skipped over by glissandi."
}

\relative c' {
  a2 \glissando
  \once \override NoteColumn #'glissando-skip = ##t
  f''4 d,
}
