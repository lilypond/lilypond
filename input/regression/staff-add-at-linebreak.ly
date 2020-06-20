\version "2.21.0"

\header {
  texidoc = "Adding a new staff at a line break doesn't crash."
}

\score {
  \new StaffGroup \relative c'' {
    \new Staff
    c1 \break
    << { c1 }
       \new Staff {
         c1
       }
    >>
    c1
  }
}
