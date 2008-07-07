
\header { texidoc = " Multimeasure rests are printed after solos, both
    for solo1 and for solo2."  }
\version "2.11.51"

\layout { ragged-right = ##t }
\new Staff 
\partcombine
\relative c''{ R1*2 | c4 r2.  | c2 r  | R1 }
\relative c' {  c2 r | R1 | c4 r2. | R1*2  }
