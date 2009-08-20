\version "2.13.4"

\header {
  texidoc = "Empty barlines do not affect spacing."
}

{
  c'4 c' \bar "" c' c' \break
  \repeat unfold 10 { c' c' c' c' \bar "" \noBreak }
}