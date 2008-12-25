\header {

  texidoc = "The @code{\voiceOne} setting is retained after
finishing the grace section."

}
\version "2.12.0"

\relative c''' {
  \voiceOne
  c4
  \grace d8 c4
}
