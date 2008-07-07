
\version "2.11.51"
\header{
  
  texidoc="
Tuplets are indicated by a bracket with a number.  There should be no
bracket if there is a beam exactly  matching  the length of the tuplet.
The bracket does not interfere with the stafflines, and the number is
centered in the gap in the bracket.

The bracket stops at the end of the stems, if the stems have the same
direction as the bracket. The endings can be adjusted with
@code{bracket-flare}.


"

}
\layout { ragged-right= ##t }


\context Voice \relative c'' {
  \times 2/3 { a8 b c }
  \times 2/3 { r8  b[ f] }
  \override TupletBracket  #'bracket-flare = #'(0.5 . 0.5)
  \times 2/3 { r8 b r8 }
  c4 |
  
  \times 3/4 { c4 c4 c4 c4 } c4 | 
  
  \time 6/8
  \times 6/9 { c8 c c c c c c c c }

}




