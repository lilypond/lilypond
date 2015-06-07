\version "2.19.21"

\header {
  texidoc = "A tuplet number associated with a kneed beam is not placed between
beam and staff where it may collide with ledger lines."
}

\layout {
  indent = 0
  ragged-right = ##t
}

\relative {
  \override Beam.auto-knee-gap = 1
  \tuplet 3/2 4 {
    c''8 c''' cis,,,
    c8 c'''' c,,,
    c''8 c c,,
    c,8 c,,, c'''
    c,8 c,, c''
    c,8 c,, c''
    \override TupletNumber.font-size = 5
    c,,8 c c''
  }
}
