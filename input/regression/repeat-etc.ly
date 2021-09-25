\version "2.23.4"

\header{
  texidoc="
Repeat constructs without alternatives can be abbreviated using \\etc .
"
}

"\\%" = \repeat percent \etc
quad = \repeat unfold 4 \etc

{
  \%4 \quad c'4 \bar "|."
}
