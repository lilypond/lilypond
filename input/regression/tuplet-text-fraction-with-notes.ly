\version "2.21.0"
\header{
  texidoc="Non-standard tuplet texts: Printing a tuplet fraction with note durations assigned to both the denominator and the numerator."
}


\context Voice \relative {
  \once \override TupletNumber.text = #(tuplet-number::fraction-with-notes (ly:make-duration 2 1) (ly:make-duration 3 0))
  \tuplet 3/2  { c''4. c4. c4. c4. }
  \once \override TupletNumber.text = #(tuplet-number::non-default-fraction-with-notes 12 (ly:make-duration 3 0) 4 (ly:make-duration 2 0))
  \tuplet 3/2  { c4. c4. c4. c4. }
}
