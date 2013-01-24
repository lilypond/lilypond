\version "2.17.11"
\header{
  texidoc="Non-standard tuplet texts: Printing a tuplet fraction with note durations assigned to both the denominator and the numerator."
}


\context Voice \relative c'' {
  \once \override TupletNumber.text = #(tuplet-number::fraction-with-notes "4." "8")
  \tuplet 3/2  { c4. c4. c4. c4. }
  \once \override TupletNumber.text = #(tuplet-number::non-default-fraction-with-notes 12 "8" 4 "4")
  \tuplet 3/2  { c4. c4. c4. c4. }
}
