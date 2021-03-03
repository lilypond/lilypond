\version "2.23.1"

\paper { ragged-right = ##t }

staff = \new Staff \fixed c' {
  \bar \testBar R1 \bar \testBar R1 \bar \testBar
}

\new Score <<
     \new PianoStaff \with { instrumentName = \testBar } << \staff \staff >>
>>
