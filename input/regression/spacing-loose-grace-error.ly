\header
{

texidoc = "Even in case of incorrect contexts (eg. shortlived
  contexts) that break linking of columns through spacing wishes,
  @code{strict-note-spacing} defaults to a robust solution.
  This test passes if it does not seg fault; instead it should
  produce three programming error messages.  Note that, in tight
  music with strict note spacing, grace notes will collide with
  normal notes.  This is expected."

}

\version "2.14.0"

%% \new Staff cause shortlived, disconnected Voice contexts
%% breaking spacing-wishes links.
\score {
  \new Staff {
    \override Score.SpacingSpanner #'strict-note-spacing = ##t
    \afterGrace c'4 {c'32 c'32 }
    c'4
  }
  \layout {
    ragged-right = ##f
  }
}
