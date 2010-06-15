\version "2.13.23"

\header {
  texidoc = "StaffGrouper interacts correctly with \RemoveEmptyStaffContext.
In both systems, there should be a large space between the staff groups."
}

\layout {
  \context {
    \RemoveEmptyStaffContext
  }
}

\paper {
  ragged-right = ##t
}

\score {
  <<
    \new StaffGroup = "G1" \with {
      \override StaffGrouper #'after-last-staff-spacing #'space = #20
    }
    <<
      \new Staff { c'1 \break c'1 \break R1 }
      \new Staff { c'1 R1 c'1 }
    >>
    \new Staff { c'1 c'1 c'1 }
  >>
}