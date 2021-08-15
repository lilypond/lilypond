\version "2.23.4"

\header {
  texidoc = "Balloons work on cross-staff grobs."
}

\layout {
  \context {
    \Voice
    \consists Balloon_engraver
  }
}

\new PianoStaff <<
  \new Staff = "1" {
    \balloonLengthOn
    \balloonGrobText Script #'(-1 . 0.3) "marcato"
    c''8\tweak padding 1 _^ 8
    \change Staff = "2"
    c''8 8
  }
  \new Staff = "2" s2
>>
