\version "2.9.12"

\header {

  texidoc = "Falls and doits can be created with bendAfter. They run
 to the next note, or to the next barline."

}

\paper {
  ragged-right = ##T
}

\relative c'' {
  \override Score.SpacingSpanner #'shortest-duration-space = #3.0
  c4-\bendAfter #+5
  c4-\bendAfter #+4
  c4-\bendAfter #+3
  c4-\bendAfter #+2
  c4-\bendAfter #+1
  c4-\bendAfter #-1
  c4-\bendAfter #-2
  c4-\bendAfter #-3
  c4-\bendAfter #-4
}
