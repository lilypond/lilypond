\version "2.19.2"
\header {
  texidoc = "Nested properties may be overridden using Scheme list
syntax.  This test performs two property overrides: the first
measure uses standard @code{\\override} syntax; the second uses a
list.
"
}

\relative c' {
  \once \override TextSpanner.bound-details.left.text = #"foo"
  c4\startTextSpan
  \once \override Tie.details.note-head-gap = #1
  c4 ~ 4 c\stopTextSpan
  
  \once \override TextSpanner.bound-details.left.text = #"foo"
  c4\startTextSpan
  \once \override Tie.details.note-head-gap = #1
  c4 ~ 4 c\stopTextSpan
}
