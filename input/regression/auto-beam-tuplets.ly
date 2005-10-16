
\version "2.7.13"

\header {
  texidoc = "Tuplet-spanner should not put (visible) brackets on
beams even if they're auto generated."
}
\layout { raggedright= ##t }

\relative c' {
  \set tupletSpannerDuration = #(ly:make-moment 1 4)
  \override TupletBracket  #'bracket-visibility = #'if-no-beam
  \times 2/3 {
    f8[ f f ] f8[ f f ] f f f f f f 
  }
}

