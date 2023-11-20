\version "2.19.21"

\header {
  texidoc = "Figured bass extender lines run between repeated bass figures.
They are switched on with @code{useBassFigureExtenders}."
}

<<
  \new Voice \relative {
    c'8 c b b a a b b |
    c^"the same with extenders" c b b a a b b
  }
  \figures {
    <6+ 4 3>4 <6 4 3> <4 3+> r |
    \set useBassFigureExtenders = ##t
    <6+ 4 3> <6 4 3> <4 3+> r
  }
>>
