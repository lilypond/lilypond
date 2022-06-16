\version "2.19.36"
\header {

  texidoc = "
Rests should not keep staves alive when
@code{\\RemoveEmptyStaves} is active.  The
following example should have only one staff.
"

}

\score {
 <<
   \new Staff {
     \partial 16 r16 | R1 | r1
   }
   \new Staff {
     \partial 16 c'16 | c'1 | c'1
   }
 >>
 \layout {
  \context {
   \Staff
   \RemoveAllEmptyStaves
  }
 }
}
