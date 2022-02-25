\version "2.21.0"

#(ly:set-option 'warning-as-error #t)
%% not sure why these warnings appear twice [dfe]
#(ly:expect-warning (G_ "default child context begins a cycle: ~a") 'Score)
#(ly:expect-warning (G_ "cannot find or create context: ~a") 'Bottom)
#(ly:expect-warning (G_ "default child context begins a cycle: ~a") 'Score)
#(ly:expect-warning (G_ "cannot find or create context: ~a") 'Bottom)

\header {
  texidoc = "A @code{\\defaultchild} cycle does not induce an endless loop.
The output of this test is not important."
}

\layout {
  \context {
    \Score
    \defaultchild "Tweedle"
  }
  \context {
    \Staff
    \name "Tweedle"
    \defaultchild "Score"
  }
}

{ c1 }
