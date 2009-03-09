\version "2.12.3"
\header{
  texidoc="Non-standard tuplet texts: Printing a tuplet fraction with note durations assigned to both the denominator and the numerator."
}


\context Voice \relative c'' {
  \once \override TupletNumber #'text = #(tuplet-number::fraction-with-notes "4." "8")
  \times 2/3  { c4. c4. c4. c4. }
  \once \override TupletNumber #'text = #(tuplet-number::non-default-fraction-with-notes 12 "8" 4 "4")
  \times 2/3  { c4. c4. c4. c4. }
}
