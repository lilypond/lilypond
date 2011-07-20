\version "2.15.6"

\header {
  texidoc = "@code{\\RemoveEmptyStaves} is defined separately from
context definitions so it can be used outside of @code{\\layout} blocks."
}

\paper {
  ragged-right = ##t
}

\new Staff \RemoveEmptyStaves {
  c'1 \break
  r1
}
