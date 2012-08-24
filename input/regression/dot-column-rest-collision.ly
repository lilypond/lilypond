\header {
  texidoc = "Dot columns do not trigger beam slanting too early.
This input should compile with no programming error message,
and the dots should be correctly placed on their rests."
}

\version "2.16.0"
\paper{ ragged-right=##t }
<<
  { e''8 e'' g'' g'' g''16[ r8. r8. g''16] } \\
  { e8 r4. c'16[ r8. <a' b' c''>8. e'16] }
>>
