\version "2.14.1"
\header {
  texidoc = "Ambitus accidentals (whether present or not) are ignored by the
slur engravers.
" 
}

\score {
  \new Voice \with { \consists Ambitus_engraver } {
    c'4( es')
  }
}
