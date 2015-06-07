
\version "2.19.21"

\header {

  texidoc = " Broken ties honor @code{minimum-length} also.  This tie
has a @code{minimum-length} of 5."

}

\paper {
  indent = 0.0\mm
  line-width = 40.0\mm
}

\relative {
  \override Tie.minimum-length = #5
  f'2. f16  f  f  f ~ | \break
  f1
}
