%-------------------8<----------------------------
\version "2.13.30"

music = {
  <c e g c' e'>4:16
  \stemUp
  \repeat tremolo 4 c'16
  \repeat tremolo 2 { c16 d }
  \repeat tremolo 4 { <c d>16 }
}

\score {
  <<
    \new Staff {
      \clef "treble_8"
      \music
    }
    \new TabStaff {
      \music
    }
  >>
}

\score {
  \new TabStaff {
    \tabFullNotation
    \music
  }
}
%-------------------8<----------------------------
