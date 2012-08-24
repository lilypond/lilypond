\version "2.16.0"

\header {

  texidoc = "
Tremolos will appear on tablature staffs only if
@code{\\tabFullNotation} is active.  Otherwise, no
tremolo indications are displayed on the TabStaff.
Also, tablature beams are the same thickness on TabStaff
and Staff."

  doctitle = "Tablature tremolo"
}

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

