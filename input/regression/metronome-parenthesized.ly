\version "2.19.21"

\header{
  texidoc="
Using an empty text in the metronome marks, one can generate parenthesized tempo marks.
"
}

\relative {
  \tempo 4=60
  c''1
  \tempo "" 4=80
  c1
}
