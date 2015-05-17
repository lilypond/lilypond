\version "2.19.21"
\header {
  texidoc = "Ledger lines should appear at every other location
for a variety of staves using both @code{line-count} and
@code{line-positions}."
}

notes = \relative {
  \time 3/4
  c'2. | d | e | f
  g2. | a | b | c
  d2. | e | f | g
  a2.
}

\new Staff {
  % upper and lower lines both odd
  #(define mylines '(-1 0 1))
  \override Staff.StaffSymbol.line-count = #(length mylines)
  \override Staff.StaffSymbol.line-positions = #mylines
  \notes
}

\new Staff {
  % upper and lower lines both even
  #(define mylines '(-2 0 2))
  \override Staff.StaffSymbol.line-positions = #mylines

  \override Staff.StaffSymbol.line-count = #(length mylines)
  \notes
}

\new Staff {
  % lower line odd, upper line even
  #(define mylines '(-1 0 2))
  \override Staff.StaffSymbol.line-positions = #mylines
  \override Staff.StaffSymbol.line-count = #(length mylines)
  \notes
}

\new Staff {
  % odd line count
  \override Staff.StaffSymbol.line-count = #5
  \notes
}

\new Staff {
  % even line count
  \override Staff.StaffSymbol.line-count = #4
  \notes
}
