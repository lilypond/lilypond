\version "2.23.6"

\header {
  texidoc ="Adjustments to @code{VoiceFollower.bound-details.left.Y}
are relative to the @code{VoiceFollower} grob's start staff.  In this
test, the lines should start and end at the exact middle of the
respective staves."
}

\paper {
  ragged-right = ##t
}

<<
  \new Staff = "top" {
    \override VoiceFollower.bound-details.left.padding = 0
    \override VoiceFollower.bound-details.left.Y = 0
    \override VoiceFollower.bound-details.right.padding = 0
    \override VoiceFollower.bound-details.right.Y = 0
    \override VoiceFollower.after-line-breaking = ##t
    d''1
    \showStaffSwitch
    \change Staff = "bottom"
    g'1
    \hideStaffSwitch
    \change Staff = "top"
    d''1
    \showStaffSwitch
    \break
    \change Staff = "bottom"
    g'1
  }
  \new Staff = "bottom" {
    s1*4
  }
>>
