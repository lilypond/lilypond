\version "2.16.0"

\header {
  texidoc = "Empty bar lines do not affect spacing."
}

{
  c'4 c' \bar "" c' c' \break
  \repeat unfold 10 { c' c' c' c' \bar "" \noBreak }
}
