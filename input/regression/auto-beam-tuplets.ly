
\version "2.17.11"

\header {
  texidoc = "Tuplet-spanner should not put (visible) brackets on
beams even if they're auto generated."
}
\layout { ragged-right= ##t }

\relative c' {
  \tupletSpan 4
  \override TupletBracket.bracket-visibility = #'if-no-beam
  \tuplet 3/2 {
    f8[ f f ] f8[ f f ] f f f f f f 
  }
}

