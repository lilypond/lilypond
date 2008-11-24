\version "2.11.65"
\header {
  texidoc = "Nested properties may be overridden using Scheme list
syntax.  This test performs two property overrides: the first
measure uses standard @code{\\override} syntax; the second uses a
list.
"
}

\relative c' {
  \once \override TextSpanner #'bound-details #'left #'text = #"foo"
  c4\startTextSpan
  \once \override Tie #'details #'note-head-gap = #1
  c4 ~ c c\stopTextSpan
  
  \once \override TextSpanner #'(bound-details left text) = #"foo"
  c4\startTextSpan
  \once \override Tie #'(details note-head-gap) = #1
  c4 ~ c c\stopTextSpan
}
