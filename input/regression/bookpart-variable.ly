\version "2.13.41"


\header {
  texidoc = "
A @code{\bookpart} variable can be inserted in a @code{\book}.
No segfault should occur in this case.
"
}

mypart = \bookpart {
  \relative c' {
    c1
  }
}

\book {
  \mypart
}
