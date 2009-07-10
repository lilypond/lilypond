\version "2.13.4"

\header{ texidoc = "The sans serif style tab clef is automatically adjusted to
                    different string spacings."
       }

guitar = \relative c {
  c4 d e f
  e4 d c2
}

\score {
  <<
    \new Staff {
      \clef "treble_8"
      \guitar
    }
    \new TabStaff {
      \clef "moderntab"
      \set TabStaff.stringTunings = #guitar-tuning
      \guitar
    }
  >>
}

\score {
  <<
    \new Staff {
      \clef "treble_8"
      \guitar
    }
    \new TabStaff {
      \clef "moderntab"
      \set TabStaff.stringTunings = #guitar-tuning
      \override TabStaff.StaffSymbol #'staff-space = #1.0 % default value is 1.5
      \guitar
    }
  >>
}