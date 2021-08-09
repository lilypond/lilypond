\version "2.19.21"
\header {
  texidoc = "Chained trills end at the next trill or bar line.
Collisions can be prevented by overriding @code{bound-details}.
"
}

\paper { ragged-right = ##f }

\relative {
  g'8 f\startTrillSpan ~
  f8 g\stopTrillSpan \startTrillSpan ~
  g8 r\stopTrillSpan r4
  \once \override TrillSpanner.to-barline = ##t
  c1\startTrillSpan ~
  c1
  \once \override TrillSpanner.bound-details.right.padding = #1.2
  c1\stopTrillSpan \startTrillSpan
  c1\stopTrillSpan\startTrillSpan
}
