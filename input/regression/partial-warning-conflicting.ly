\version "2.27.2"

\header {
  texidoc = "@code{\\partial} commands with conflicting durations trigger a
warning.  This test uses @code{\\partial@tie{}4} and then @code{\\partial 4*2}
at the same time step."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "conflict with event: `%s'") "partial-event")
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "discarding event: `%s'") "partial-event")

\fixed c' {
  \partial 4 \partial 4*2 c4 c4
}
