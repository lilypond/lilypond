\version "2.23.6"

\header {
  texidoc = "@code{\magnifyStaff} also works for @code{Dynamics} contexts.
This test should print a huge forte dynamic."
}

<<
  \new Staff { c'1 }
  \new Dynamics \with {
    \magnifyStaff #2
  }
  { s1\f }
>>
