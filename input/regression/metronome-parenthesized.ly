\version "2.12.0"

\header{
  texidoc="
Using an empty text in the metronome marks, one can generate parenthesized tempo marks.
"
}

\relative c'' {
  \tempo 4=60
  c1
  \tempo "" 4=80
  c1
}
