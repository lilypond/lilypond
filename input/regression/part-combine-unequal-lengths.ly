\version "2.21.0"

\header {
  texidoc ="The part combiner can combine parts of unequal lengths."
}

\layout { ragged-right = ##t }

\new Staff { %% based on the example in Issue 913
  c'1
  \partCombine { e' r } { c' }
  \partCombine { b' } {}
  \partCombine {} { f' }
}
