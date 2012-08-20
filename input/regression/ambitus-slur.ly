\version "2.16.0"
\header {
  texidoc = "Ambitus accidentals (whether present or not) are ignored by the
slur engravers.
" 
}

\score {
  \new Voice \with { \consists "Ambitus_engraver" } {
    c'4( es')
  }
}
