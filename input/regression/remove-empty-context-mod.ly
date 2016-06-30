\version "2.19.46"

\header {
  texidoc = "@code{\\RemoveEmptyStaves} is defined separately from
context definitions so it can be used outside of @code{\\layout} blocks."
}

\paper {
  ragged-right = ##t
}

\new Staff \with \RemoveEmptyStaves {
  c'1 \break
  r1
}
