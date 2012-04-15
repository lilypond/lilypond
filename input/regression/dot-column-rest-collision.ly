\header {
  texidoc = "Dot columns do not trigger beam slanting too early.
This input should compile with no programming error message,
and the dots should be correctly placed on their rests."
}

\version "2.14.0"
\paper{ ragged-right=##t }
<<
  { e''8 e'' g'' g'' e''8[ r8. e''16 g''8] } \\
  { e8 r4. c'8[ r8. f'16 e'8] }
>>
