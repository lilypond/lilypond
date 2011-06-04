\version "2.14.0"

\header {
  texidoc="
Setting @code{includeGraceNotes} enables lyrics syllables to be
assigned to grace notes.
"
}

\relative c' {
  f4 \appoggiatura a32 b4
  \grace { f16[ a16] } b2
  \afterGrace b2 { f16[ a16] }
  \appoggiatura a32 b4
  \acciaccatura a8 b4
}
\addlyrics {
  normal
  \set includeGraceNotes = ##t
  case,
  gra -- ce case,
  after -- grace case,
  \set ignoreMelismata = ##t
  app. case,
  acc. case.
}
