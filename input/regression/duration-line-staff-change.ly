\version "2.23.5"

\header {
  texidoc = "Duration lines work across staff changes."
}

<<
  \new Staff = upper \new Voice \with {
    \consists Duration_line_engraver
  }
  {
    c'1\-
    \change Staff = lower
    c'1
    \change Staff = upper
  }
  \new Staff = lower {
    s1*2
  }
>>
