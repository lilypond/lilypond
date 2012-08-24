\version "2.16.0"
\header {
  texidoc = "A piano context included within a staff group should
cause the piano brace to be drawn to the left of the staff angle
bracket."
}

\layout {ragged-right = ##t}


{
  \context StaffGroup <<
    c4
    \context PianoStaff <<
      \new Staff d
      \new Staff e
    >>
  >>
}



