\version "2.19.21"

\header {

  texidoc = "Falls and doits can be created with bendAfter. They run
 to the next note, or to the next bar line.  Microtone bends (i.e.,
 \bendAfter #3.5) are also supported."

}

\paper {
  ragged-right = ##T
}

\relative {
  \override Score.SpacingSpanner.shortest-duration-space = #3.0
  c''4-\bendAfter #+5
  c4-\bendAfter #+4
  c4-\bendAfter #+3
  c4-\bendAfter #+2
  c4-\bendAfter #+1
  c4-\bendAfter #-1
  c4-\bendAfter #-2
  c4-\bendAfter #-3
  c4-\bendAfter #-4
  c4-\bendAfter #-5
  c4-\bendAfter #3.5
  c4-\bendAfter #2.5
  c4-\bendAfter #1.5
  c4-\bendAfter #0.5
  c4-\bendAfter #-0.5
  c4-\bendAfter #-1.5
  c4-\bendAfter #-2.5
  c4-\bendAfter #-3.5
}
