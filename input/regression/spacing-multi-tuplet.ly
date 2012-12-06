\version "2.17.6"

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
      \times 2/10 {  c8[ c c c c c c c c c] } 
      \times 2/10 {  c[  c c c c c c c c c] } 
    }
    \new Staff  \context Voice { 
      \times 2/11 {  c8[ c c c c c c c c c c] } 
      \times 2/11 {  c[  c c c c c c c c c c] } 
    }
  >>
}



