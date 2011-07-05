\version "2.14.0"

\header {
  texidoc = "In a TabStaff, the chord repetition function needs
to save the string information. This is handled by
@code{\\tabChordRepetition}."
}

\tabChordRepetition

Guitar = \relative c' {
  r8 < gis\4 cis\3 b\2 > ~ q4 q8 ~ q q4
}

\score {
  \new StaffGroup <<
    \new Staff {
      \new Voice {
        \clef "treble_8"
        \override Voice.StringNumber #'transparent = ##t
        \Guitar
      }
    }
    \new TabStaff {
      \new TabVoice {
        \Guitar
      }
    }
  >>
}
