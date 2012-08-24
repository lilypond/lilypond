
\header {
texidoc = "Tied notes with accidentals do not cause problems with spacing."
}

\version "2.16.0"

\relative c' {
  \clef treble
  \time 3/4
  c8 b2  <g b des f>8 ~ |
  <g b des f>8
}

