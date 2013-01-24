\version "2.17.11"
\header{
  texidoc="Non-standard tuplet texts: Appending a note value to the normal text and to the fraction text."
}
\layout { ragged-right= ##t }


\context Voice \relative c'' {
  \once \override TupletNumber.text = #(tuplet-number::append-note-wrapper tuplet-number::calc-denominator-text "4")
  \tuplet 3/2  { c8 c8 c8 c8 c8 c8 }
  \once \override TupletNumber.text = #(tuplet-number::append-note-wrapper tuplet-number::calc-fraction-text "4")
  \tuplet 3/2  { c8 c8 c8 c8 c8 c8 }
}
