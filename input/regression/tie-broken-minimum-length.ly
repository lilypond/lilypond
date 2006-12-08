
\version "2.11.0"

\header {

  texidoc = " Broken ties honor @code{minimum-length} also.  This tie
has a @code{minimum-length} of 5."

}

\relative {
  \override Tie #'minimum-length = #5
  f2. f16  f  f  f ~ | \break
  f1
}

\paper {
  indent = 0.0\mm
  line-width = 40.0\mm
}
