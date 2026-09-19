\version "2.27.5"

\header { texidoc = "
Prefatory matter will move backwards across multiple grace times and
multiple @code{<<@dots{}>>} constructs." }

<<
  \new Staff { \grace c'4 1 }
  \new StaffGroup << \new Staff { \grace 8 1 }
		     \new Staff { \clef bass 1 }>>
>>
