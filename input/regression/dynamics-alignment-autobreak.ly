\version "2.15.7"

\header {
  texidoc = "If a dynamic has an explicit direction that differs from the 
dynamic line spanner's direction, automatically break the dynamic line spanner.
"
}

\relative c' {
  c1^\<
  c1_\>
  f,1\p

  c'1^\<
  c1_\p^\>
  c1\!
}
