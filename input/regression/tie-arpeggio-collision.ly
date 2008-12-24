
\header {

  texidoc = "Advanced tie chord formatting also works with arpegiated
ties.  Due to arpeggios, tie directions may be changed relative to the
unarpegiated case."

  
}

\version "2.12.0"


\layout { ragged-right = ##t }
\new Staff \relative c'' {
  <e c a f>2~ <e c a f> |
  \set tieWaitForNote = ##t
  e8~ c~ a~ f~ <e' c a f>2 |
  f,8~ a~ c~ e~ <f, a c e>2 |
}

