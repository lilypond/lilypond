\header {
  texidoc = "Slurs avoid clefs, but don't avoid barlines."
  }

\paper {
  ragged-right = ##t

}
\version "2.10.0"

\new Staff { \clef bass c4^( \clef "G" g'4) c''1_( f'') } 
