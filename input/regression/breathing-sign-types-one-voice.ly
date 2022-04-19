\version "2.23.9"

\header {
  texidoc="This test shows the predefined values available for context
properties that specify a type of @code{BreathingSign}.  The dotted
lines are bar lines."
}

\include "breathing-sign-types.ily"

\new ChoirStaff \with {
  measureBarType = "!"
} <<
  \new Staff \with {
    instrumentName = "default"
  } {
    \music
  }
  \new Staff \with {
    instrumentName = "DOWN"
    \override BreathingSign.direction = #DOWN
    \omit TextScript
  } \music
>>
