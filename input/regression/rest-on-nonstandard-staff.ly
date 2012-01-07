\version "2.15.18"

\header {
  texidoc = "half rests should lie on a staff line, whole rests should hang
  from a staff line by default even for non-standard staves, except when
  the position is set by pitch."
}


\layout {
  ragged-right = ##t
  indent = 0.0
}

\new StaffGroup <<
  \new Staff {
    r2
    g'2\rest
    r1
    g'1\rest
  }

  \new Staff {
    \override Staff.StaffSymbol #'line-positions = #'(-4 -2 0 2)
    r2
    g'2\rest
    r1
    g'1\rest
  }

  \new Staff {
    \override Staff.StaffSymbol #'line-count = #4
    r2
    g'2\rest
    r1
    g'1\rest
  }
>>
