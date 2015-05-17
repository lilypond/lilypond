\version "2.19.21"
\header {
  texidoc = "Beam collision can be tweaked to only apply to the grobs
within the beam's original voice."
}

\relative {
  \time 5/4
  << { c'8[ s c ] } \\ { s8 c' s8 } >>
  c,[ des' ]
  \override Staff.Beam.collision-voice-only = ##t
  << { c,8[ s c ] } \\ { s8 c' s8 } >>
  c,[ des'! ]
}
