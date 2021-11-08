\version "2.23.5"

\header {
  texidoc = "Voice followers can be broken across more than
two systems."
}

\paper {
  ragged-right = ##t
}

<<
  \new Staff {
    \override VoiceFollower.after-line-breaking = ##t
    c''1
    \showStaffSwitch
    \break
    s1
    \break
    \change Staff = lower
    c'1
  }
  \new Staff = lower {
    s1*2
  }
>>
