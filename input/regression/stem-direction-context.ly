\header {

  texidoc = "Stem directions for notes on the middle staff line are
  determined by the directions of their neighbors."

}

\version "2.12.0"
\relative c'' \new Voice \with {
     \consists "Melody_engraver"
     \override Stem #'neutral-direction = #'()
} {
  c4 b c b
  c c c c
  b a b a
} 
  
  
