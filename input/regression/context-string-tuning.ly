\version "2.15.16"

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
    \set stringTunings = #custom-tuning
    \mynotes
  }
>>

