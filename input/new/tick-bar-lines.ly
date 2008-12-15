\version "2.11.65"
\header {
  lsrtags = "staff-notation"
  texidoc = "
'Tick' bar lines are often used in music where the bar line is used
only for coordination and is not meant to imply any rhythmic stress.
"
  doctitle = "Tick bar lines"
}
\relative c' {
  c4 d e f \bar "'"
  g4 f e d \bar "'" 
  c4 d e f \bar "'"
  g4 f e d
  \bar "|."
}
