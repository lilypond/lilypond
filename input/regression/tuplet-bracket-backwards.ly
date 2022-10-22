\version "2.23.81"

\header {
  texidoc = "
When the tuplet number is wider than the bracket, no tuplet bracket
is printed.
"
}

\tuplet 1000/999 { c'8 d' }

\tuplet 1000/999 { c'8 d' e' }

{
  \override TupletNumber.text = #tuplet-number::calc-fraction-text
\tuplet 9/8 { c'8 d' }
}

{
  \override TupletNumber.text = #tuplet-number::calc-fraction-text
\tuplet 9/8 { c'8 d' e'8 }
}

\relative {
  \once \override TupletNumber.text =
    #(tuplet-number::fraction-with-notes
      (ly:make-duration 2 0) (ly:make-duration 2 0))
  \tuplet 5/4  { c'4 c }
}

\relative {
  \once \override TupletNumber.text =
    #(tuplet-number::fraction-with-notes
      (ly:make-duration 2 0) (ly:make-duration 2 0))
  \tuplet 5/4  { c'4 c c }
}

\relative {
  \once \override TupletNumber.text =
    #(tuplet-number::fraction-with-notes
      (ly:make-duration 2 0) (ly:make-duration 2 0))
  \tuplet 5/4  { c'4 c c c }
}

