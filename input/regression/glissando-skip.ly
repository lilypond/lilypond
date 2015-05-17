\version "2.19.21"

\header {
  texidoc = "@code{NoteColumn} grobs can be skipped over by glissandi.
"
}

\relative {
  a2 \glissando
  \once \override NoteColumn.glissando-skip = ##t
  f''4 d,
}
