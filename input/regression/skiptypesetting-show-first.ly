\version "2.25.24"

\header {
  texidoc = "@code{showFirstLength} will only show the first bit of a score"
}

#(ly:set-option 'warning-as-error #t)

\paper {
  ragged-right = ##t
}

showFirstLength = R1*2

\fixed c' {
  c1
  d8 8 8 8 8 8 8 8 % 8ths for issue 6657
  f1
}
