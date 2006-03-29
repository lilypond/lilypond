\version "2.7.39" \header{


  texidoc = "Concurrent tuplets should be equidistant on all staffs.
Such equidistant spacing is it at odds with elegant engraver spacing;
hence it must be switched on explicitly with the
@code{uniform-stretching} property of @code{SpacingSpanner}.
"

}



\relative c' { 
  \new Score \with
  {
    \override SpacingSpanner #'uniform-stretching = ##t 
  }
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



