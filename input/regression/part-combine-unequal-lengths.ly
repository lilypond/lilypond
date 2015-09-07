\version "2.19.28"

\header {
  texidoc ="The part combiner can combine parts of unequal lengths."
}

\layout { ragged-right = ##t }

\new Staff { %% based on the example in Issue 913
  c'1
  \partcombine { e' r } { c' }
  \partcombine { b' } {}
  \partcombine {} { f' }
}
