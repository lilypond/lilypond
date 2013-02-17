\version "2.17.11"

 \header{

  texidoc = "Concurrent tuplets should be equidistant on all staves.
Such equidistant spacing is at odds with elegant engraver spacing;
hence it must be switched on explicitly with the
@code{uniform-stretching} property of @code{SpacingSpanner}."
}

\layout{
  \context{
    \Score
    \override SpacingSpanner.uniform-stretching = ##t
  }
}

\relative c' { 
  \context StaffGroup << 
    \new Staff  \context Voice { 
      \tuplet 10/2 {  c8[ c c c c c c c c c] } 
      \tuplet 10/2 {  c[  c c c c c c c c c] } 
    }
    \new Staff  \context Voice { 
      \tuplet 11/2 {  c8[ c c c c c c c c c c] } 
      \tuplet 11/2 {  c[  c c c c c c c c c c] } 
    }
  >>
}



