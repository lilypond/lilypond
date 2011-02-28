\version "2.13.52"

\header {
  texidoc = "StaffGroups can be nested, in which case the inner StaffGroup wins."
}

\score {
 <<
   \new StaffGroup <<
     \new Staff {
       c'1
     }
     \new StaffGroup \with {
       \override StaffGrouper #'staffgroup-staff-spacing #'basic-distance = #20
     } <<
       \new Staff {
         c'1
       }
       \new Staff {
         c'1
       }
     >>
     \new Staff {
       c'1
     }
   >>
 >>
}

