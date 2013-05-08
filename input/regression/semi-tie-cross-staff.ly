\version "2.17.18"

\header {
  texidoc = "Cross-staff @code{RepeatTie} and @code{LaissezVibrerTie}
do not trigger programming errors for circular dependencies in direction.
"
}

<<
  \new Staff = "up" \relative c' {
    f8
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
