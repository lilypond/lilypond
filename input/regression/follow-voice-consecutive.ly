\version "2.11.51"

\header {
  texidoc= "The voice follower is not confused when set for consecutive
sets of staff switches."
}

\new PianoStaff \relative c' <<
  \new Staff = "one" {
    c4
    \showStaffSwitch
    \change Staff = two
    a4
    \hideStaffSwitch
    \change Staff = one
    c4
    \showStaffSwitch
    \change Staff = two
    a4
    \hideStaffSwitch
  }
  \new Staff = "two" { \clef bass s1 }
>>
