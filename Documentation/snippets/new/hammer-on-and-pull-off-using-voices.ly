\version "2.16.0"

\header {
  lsrtags = "editorial-annotations, fretted-strings"

  texidoc = "
The arc of hammer-on and pull-off is upwards in voices one and three and
downwards in voices two and four:

"
  doctitle = "Hammer on and pull off using voices"
}

\new TabStaff {
  \relative c' {
    << { \voiceOne g2( a) }
    \\ { \voiceTwo a,( b) }
    >> \oneVoice
  }
}
