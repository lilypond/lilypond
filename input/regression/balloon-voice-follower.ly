\version "2.23.6"

\header {
  texidoc = "Balloons can be attached to voice followers."
}

\layout {
  \context {
    \Score
    \consists Balloon_engraver
    \override BalloonText.padding = 0.4
  }
  \context {
    \Staff
    \override VoiceFollower.bound-details.left.padding = 2
    \override VoiceFollower.bound-details.right.padding = 2
  }
}

<<
  \new Staff {
    \showStaffSwitch
    c'1
    \change Staff = down
    \balloonGrobText VoiceFollower #'(2 . 0) \markup \italic \small "Same voice continued"
    b1

  }
  \new Staff = down {
    \clef bass
    s1
  }
>>
