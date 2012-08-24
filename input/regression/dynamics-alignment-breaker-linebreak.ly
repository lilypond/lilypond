\version "2.16.0"

\header {
  texidoc = "@code{\\breakDynamicSpan} shall also work if a 
dynamic spanner crosses a line break.
"
}

\relative c' {
  % spanner really crosses linebreak:
  c1\<\breakDynamicSpan c'' \break
  c,,1
  % new spanner immediately after linebreak (with broken spanner):
  c''1\>\breakDynamicSpan \break
  c,,1\<
  f,1\breakDynamicSpan\p
}
