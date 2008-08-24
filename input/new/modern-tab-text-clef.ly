\version "2.11.57"

\header {
  lsrtags = "staff-notation, fretted-strings"

  texidoc = "
Use a markup text to replace the (TAB) clef glyph with a modern font.

"
  doctitle = "Modern TAB text clef"
}

TAB = \markup {
  \raise #1.5
  \sans
  \bold
  \huge
  \override #'(baseline-skip . 2.5)
  \center-column {
    T
    A
    B
  }
}

\new TabStaff {
  \override Staff.Clef #'stencil = #(lambda (grob)
    ly:clef::print (grob-interpret-markup grob TAB))
  a
}
