\version "2.19.21"

\header{ texidoc = "Sans serif style tab clefs are supported by @code{\\clef moderntab}.
                    This alternative clef supports four- to seven-stringed instruments
                    and is scaled automatically."
       }

bass = \relative {
  c,4 d e f
  e4 d c2
}

guitar = \relative {
  c4 d e f
  e4 d c2
}

\score {
  <<
    \new Staff {
      \clef "bass_8"
      \bass
    }
    \new TabStaff {
      \clef "moderntab"
      \set TabStaff.stringTunings = #bass-four-string-tuning
      \bass
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
      \set TabStaff.stringTunings = #guitar-seven-string-tuning
      \guitar
    }
  >>
}
