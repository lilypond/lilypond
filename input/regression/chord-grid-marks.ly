\version "2.23.10"

\header {
  texidoc = "Various kinds of marks can be used within @code{ChordGrid}
contexts."
}

\paper {
  indent = 0
  ragged-right = ##f
}

\new ChordGrid {
  \tempo Swing
  c1
  \mark \default
  c1
  \jump "Da capo"
  c1
  \mark \markup \bold { \eyeglasses !! }
  c1
  \fine
}
