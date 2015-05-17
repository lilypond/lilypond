\version "2.19.21"

\header {
  texidoc = "@code{\\breakDynamicSpan} work whether it is placed together 
with the start or the end of a spanner.  Both lines should be identical.
"
}

\relative {
  c1\< c''
  % break directly before and after \> :
  c,1\breakDynamicSpan\>\breakDynamicSpan 
  f,1\p \break

  c1\<\breakDynamicSpan c''
  c,1\>
  f,1\breakDynamicSpan\p
}
