\version "2.16.0"

\header {
  texidoc = "In a TabStaff, the chord repetition function needs
to retain string and fingering information.  Using
@code{\\tabChordRepeats} achieves that, in contrast to the music
on the main staff."
}

Guitar = \relative c' {
  r8 < gis-6 cis-6 b-0 > ~ q4 q8 ~ q q4
}

\score {
  \new StaffGroup <<
    \new Staff {
      \new Voice {
        \clef "treble_8"
        \Guitar
      }
    }
    \new TabStaff {
      \new TabVoice {
        \tabChordRepeats \Guitar
      }
    }
  >>
}
