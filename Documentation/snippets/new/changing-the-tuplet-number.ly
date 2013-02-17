\version "2.17.11"

\header {
  lsrtags = "rhythms"

  texidoc = "
By default, only the numerator of the tuplet number is printed over the
tuplet bracket. Alternatively, num:den of the tuplet number may
be printed, or the tuplet number may be suppressed altogether.

"
  doctitle = "Changing the tuplet number"
}


\relative c'' {
  \tuplet 3/2 { c8 c c }
  \tuplet 3/2 { c8 c c }
  \override TupletNumber.text = #tuplet-number::calc-fraction-text
  \tuplet 3/2 { c8 c c }
  \override TupletNumber.stencil = ##f
  \tuplet 3/2 { c8 c c }
}
