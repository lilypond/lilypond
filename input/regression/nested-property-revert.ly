\version "2.14.0"

\header {
  texidoc = "
If a nested property revert follows an override in the same grob for
a different property, the nested property's default setting should not
be evicted from the property alist.
"
}

\relative c' {
  c1\startTrillSpan
  c1\stopTrillSpan
  \override TrillSpanner #'color =  #red
  \revert TrillSpanner #'(bound-details left text)
  c1\startTrillSpan
  c1\stopTrillSpan
}
