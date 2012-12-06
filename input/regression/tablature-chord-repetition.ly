\version "2.17.6"

\header {
  texidoc = "In a TabStaff, the chord repetition function needs
to save the string information.  The obsolete function
@code{\\tabChordRepetition} establishes this setting score-wide.
Nowadays, you would rather use just @code{\\tabChordRepeat} on the
music in the tabstaff, not affecting other contexts."
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
        \override Voice.StringNumber.transparent = ##t
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
