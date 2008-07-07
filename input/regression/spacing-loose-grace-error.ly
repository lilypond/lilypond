\header
{
  
texidoc = "Even in case of incorrect contexts (eg. shortlived
  contexts) that break linking of columns through spacing wishes,
  @code{strict-note-spacing} defaults to a robust solution."

}

\version "2.11.51"


%% \new Staff cause shortlived, disconnected Voice contexts
%% breaking spacing-wishes links.
\new Staff {
  \override Score.SpacingSpanner #'strict-note-spacing = ##t
  \afterGrace c'4 {c'32 c'32 }
  c'4
}
