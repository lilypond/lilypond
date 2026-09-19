\version "2.27.4"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Polymetric time signatures are handled properly when grace timing gets involved."
}

\fixed c' <<
  \new Staff {
    \time 3/4
    \grace c8 c2.
  }
  \new Staff {
    \context Staff \polymetric \time 6/8
    c2.
  }
>>
