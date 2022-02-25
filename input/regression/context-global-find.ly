\version "2.21.0"

\header {
  texidoc = "User code is not allowed to access the Global context.
  The visual output of this test is not important."
}

#(ly:set-option 'warning-as-error #t)
%% once for \context and once for \set
#(ly:expect-warning (G_ "cannot find or create context: ~a") 'Global)
#(ly:expect-warning (G_ "cannot find or create context: ~a") 'Global)

\context Global {
  \set Global.instrumentName = "Global"
  s1
}
