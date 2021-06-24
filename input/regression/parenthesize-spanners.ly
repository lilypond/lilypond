\version "2.23.4"

\header {
  texidoc = "Parenthesizing spanners is supported."
}

{
  c'\parenthesize\< d'2.\!
  \parenthesize R1
  c'1\parenthesize\< \repeat unfold 20 { 1 } 1\!
}
