\header {
  texidoc = "Slurs avoid clefs, but don't avoid barlines."
  }

\paper {
  ragged-right = ##t

}
\version "2.11.51"

\new Staff { \clef bass c4^( \clef "G" g'4) s2 c''1_( f'') } 
