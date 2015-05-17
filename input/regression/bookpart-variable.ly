\version "2.19.21"


\header {
  texidoc = "
A @code{\\bookpart} variable can be inserted in a @code{\\book}.
No segfault should occur in this case.
"
}

mypart = \bookpart {
  \relative {
    c'1
  }
}

\book {
  \mypart
}
