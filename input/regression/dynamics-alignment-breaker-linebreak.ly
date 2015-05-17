\version "2.19.21"

\header {
  texidoc = "@code{\\breakDynamicSpan} shall also work if a 
dynamic spanner crosses a line break.
"
}

\relative {
  % spanner really crosses linebreak:
  c'1\<\breakDynamicSpan c'' \break
  c,,1
  % new spanner immediately after linebreak (with broken spanner):
  c''1\>\breakDynamicSpan \break
  c,,1\<
  f,1\breakDynamicSpan\p
}
