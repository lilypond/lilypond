
\version "2.3.4"

\header { texidoc ="@cindex Slur Beautiful
The curvature of a slur is adjusted to stay away from note heads and 
stems. When the curvature would increase much, the slur is reverted 
to its default shape.  The Slur's property @code{beautiful} (which is 
loosely related to the enclosed area between the slur and the notes)
controls the transition point, and by increasing that value you may
keep slurs more curved.
"
}

\score {  {\relative c' {
  \stemDown \slurUp
  c16( a' f' a a f a, c,)
  c( a' f' a a f d, c)
  \override Slur  #'beautiful = #5.0
  c( a' f' a a f d, c)
}}
\paper { raggedright = ##t }
}
