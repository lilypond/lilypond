\version "2.19.21"

\header {
  texidoc= "The voice follower is not confused when set for consecutive
sets of staff switches."
}

\new PianoStaff \relative <<
  \new Staff = "one" {
    c'4
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
