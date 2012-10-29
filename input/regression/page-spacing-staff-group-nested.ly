\version "2.17.6"

\header {
  texidoc = "StaffGroups can be nested, in which case the inner StaffGroup wins."
}

\score {
 <<
   \new StaffGroup \with {
     \override StaffGrouper.staffgroup-staff-spacing.basic-distance = #15
   }
   <<
     \new Staff {
       c'1
     }
     \new StaffGroup \with {
       \override StaffGrouper.staffgroup-staff-spacing.basic-distance = #20
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

