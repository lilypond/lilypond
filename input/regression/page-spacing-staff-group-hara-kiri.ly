\version "2.17.6"

\header {
  texidoc = "StaffGrouper interacts correctly with @code{\RemoveEmptyStaves}.
In both systems, there should be a large space between the staff groups."
}

\layout {
  \context {
    \Staff
    \RemoveEmptyStaves
  }
}

\paper {
  ragged-right = ##t
}

\score {
  <<
    \new StaffGroup = "G1" \with {
      \override StaffGrouper.staffgroup-staff-spacing.basic-distance = #20
    }
    <<
      \new Staff { c'1 \break c'1 \break R1 }
      \new Staff { c'1 R1 c'1 }
    >>
    \new Staff { c'1 c'1 c'1 }
  >>
}
