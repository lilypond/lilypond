\version "2.14.0"

\header {
  texidoc="
Using \contextStringTuning does not break compiling.
"
}

mynotes = {
  c'4 e' g' c'' |
  e''4 g'' b'' c'''
}

<<
  \new Staff {
    \clef treble
    \mynotes
  }
  \new TabStaff {
    \contextStringTuning #'custom-tuning <c' g' d'' a''>
    \mynotes
  }
>>

