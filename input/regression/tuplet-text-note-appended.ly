\version "2.21.0"
\header{
  texidoc="Non-standard tuplet texts: Appending a note value to the normal text and to the fraction text."
}
\layout { ragged-right= ##t }


\context Voice \relative {
  \once \override TupletNumber.text = #(tuplet-number::append-note-wrapper tuplet-number::calc-denominator-text (ly:make-duration 2 0))
  \tuplet 3/2  { c''8 c8 c8 c8 c8 c8 }
  \once \override TupletNumber.text = #(tuplet-number::append-note-wrapper tuplet-number::calc-fraction-text (ly:make-duration 2 0))
  \tuplet 3/2  { c8 c8 c8 c8 c8 c8 }
}
