\version "2.25.35"

\header {
  texidoc = "Empty bar lines do not affect spacing."
}

{
  c'4 c' \bar "" c' c' \break
  \*10 { c' c' c' c' \bar "" \noBreak }
}
