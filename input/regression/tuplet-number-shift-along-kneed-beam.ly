\version "2.19.21"

\header {
  texidoc = "Tuplet numbers will maintain a constant distance from
kneed beams when offset horizontally."
}

\layout {
  indent = 0
  ragged-right = ##f
}

\relative {
  \tuplet 3/2 4 {
    c'8 g''' a
    \once \offset X-offset 2 TupletNumber
    c,,, g''' a
    \once \offset X-offset #-2 TupletNumber
    c,,, g''' a
    \once \offset X-offset 6 TupletNumber
    c,,, g''' a
  }
}
