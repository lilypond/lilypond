\version "2.25.6"

\header {
  texidoc = "The argument of @code{\\initialContextFrom} is susceptible to
modification by tags.  The expected output is a single measure with ``PASS'' in
the left margin."
}

foo = {
  \tag "to-remove" {
    \context Staff \with { instrumentName = "FAIL" } <>
  }
  \context Staff \with { instrumentName = "PASS" } R1
}

{
  \removeWithTag "to-remove" { \initialContextFrom \foo }
  \foo
}
