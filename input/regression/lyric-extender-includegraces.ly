\version "2.19.21"

\header {
  texidoc="
If @code{includeGraceNotes} is enabled, lyric extenders work as
expected also for syllables starting under grace notes.
"
}

\relative {
  c'2 \grace { c16( d e f } g2)
  f1
}
\addlyrics {
  \set includeGraceNotes = ##t
  _ Ah __ fa
}
