\version "2.16.0"

\header {
  lsrtags = "expressive-marks, staff-notation, tweaks-and-overrides"
  doctitle = "Glissandi can skip grobs"
  texidoc = "@code{NoteColumn} grobs can be skipped over by glissandi."
}

\relative c' {
  a2 \glissando
  \once \override NoteColumn #'glissando-skip = ##t
  f''4 d,
}
