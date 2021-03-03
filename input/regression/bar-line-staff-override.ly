\version "2.23.1"

\header {
  texidoc="This test exercises bar lines that are overridden in
various built-in staves."
}

measure = \fixed c' { c4 d e f | }

piece = \fixed c' {
  \measure % default measure bar here
  \measure \bar "|"
  \measure \bar "||"
  \measure \bar "|."
}

\new Score \new Staff <<
  s1*0^"Staff"
  \piece
>>

\new Score \new GregorianTranscriptionStaff <<
  s1*0^"GregorianTranscriptionStaff"
  \piece
>>

\new Score \new KievanStaff <<
  s1*0^"KievanStaff"
  \piece
>>

\new Score \new MensuralStaff <<
  s1*0^"MensuralStaff"
  \piece
>>

\new Score \new PetrucciStaff <<
  s1*0^"PetrucciStaff"
  \piece
>>

\new Score \new VaticanaStaff <<
  s1*0^"VaticanaStaff"
  \piece
>>
