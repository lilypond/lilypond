\version "2.17.6"

#(ly:expect-warning (G_ "Cannot determine neighbors for floating column. "))
#(ly:expect-warning (G_ "Loose column does not have right side to attach to."))
#(ly:expect-warning (G_ "Loose column does not have right side to attach to."))

\header {

texidoc = "Even in case of incorrect contexts (eg. shortlived
  contexts) that break linking of columns through spacing wishes,
  @code{strict-note-spacing} defaults to a robust solution.
  This test passes if it does not seg fault; instead it should
  produce three programming error messages.  Note that, in tight
  music with strict note spacing, grace notes will collide with
  normal notes.  This is expected."

}

%% \new Staff cause shortlived, disconnected Voice contexts
%% breaking spacing-wishes links.
\score {
  \new Staff {
    \override Score.SpacingSpanner.strict-note-spacing = ##t
    \afterGrace c'4 {c'32 c'32 }
    c'4
  }
  \layout {
    ragged-right = ##f
  }
}
