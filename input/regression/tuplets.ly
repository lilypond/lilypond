
\version "2.19.21"
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


\context Voice \relative {
  \tuplet 3/2 { a'8 b c }
  \tuplet 3/2 { r8  b[ f] }
  \override TupletBracket.bracket-flare = #'(0.5 . 0.5)
  \tuplet 3/2 { r8 b r8 }
  c4 |
  
  \tuplet 4/3 { c4 c4 c4 c4 } c4 | 
  
  \time 6/8
  \tuplet 9/6 { c8 c c c c c c c c }

}




