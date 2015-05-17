\version "2.19.21"

\header {
  texidoc = "Cross-staff @code{RepeatTie} and @code{LaissezVibrerTie}
do not trigger programming errors for circular dependencies in direction.
"
}

<<
  \new Staff = "up" \relative {
    f'8
    \change Staff = "down"
    c\laissezVibrer eeses
    \change Staff = "up"
    f
    f8
    \change Staff = "down"
    c eeses!\repeatTie
    \change Staff = "up"
    f
  }
  \new Staff = "down" { \clef bass s1 }
>>
