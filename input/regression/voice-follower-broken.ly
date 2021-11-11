\version "2.23.5"

\header {
  texidoc = "Voice followers have acceptable slopes across
lines breaks."
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
    \change Staff = lower
    c'1
  }
  \new Staff = lower {
    s1*2
  }
>>
