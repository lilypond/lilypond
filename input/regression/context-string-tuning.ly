\version "2.16.0"

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
    #(define custom-tuning #{ \stringTuning <c' g' d'' a''> #})
    \set Staff.stringTunings = #custom-tuning
    \mynotes
  }
>>

