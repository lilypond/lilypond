\version "2.13.1"
\header{
  texidoc="Non-standard tuplet texts: Printing other tuplet fractions than the ones actually assigned."
}
\layout { ragged-right= ##t }


\context Voice \relative c'' {
  \once \override TupletNumber #'text = #(tuplet-number::non-default-tuplet-denominator-text 7)
  \times 2/3  { c4. c4. c4. c4. }
  \once \override TupletNumber #'text = #(tuplet-number::non-default-tuplet-fraction-text 12 7)
  \times 2/3  { c4. c4. c4. c4. }
  \once \override TupletNumber #'text = #(tuplet-number::append-note-wrapper (tuplet-number::non-default-tuplet-fraction-text 12 7) "8")
  \times 2/3  { c4. c4. c4. c4. }
}
