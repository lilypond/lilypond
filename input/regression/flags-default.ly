\version "2.23.14"
#(set-global-staff-size 17)

\header {
  texidoc = "Default flag styles: '(), 'mensural and 'no-flag.
  Compare all three methods to print them: (1) C++ default implementation,
  (2) Scheme implementation using the 'style grob property and
  (3) setting the 'flag property explicitly to the desired Scheme function.
  All three systems should be absolutely identical."
}

\paper {
  line-width = 18\cm
}

% test notes, which will be shown in different style:
testnotes = { \autoBeamOff
  c'8 d'16 c'32 d'64 \acciaccatura {c'8} d'64
  c''8 d''16 c''32 d''64 \acciaccatura {c''8} d''64
}

% Old settings: style set to default, 'mensural, 'no-flag; using the
% default C++ function ly:stem::calc-stem
{
  \time 2/4

  \textMark "Default flags (C++)"
  \testnotes

  \textMark "Symbol: 'mensural (C++)"
  \override Flag.style = #'mensural
  \testnotes

  \textMark "Symbol: 'no-flag (C++)"
  \override Flag.style = #'no-flag
  \testnotes
}

% The same, but using the Scheme implementation of default-flag
{
  \time 2/4

  \override Flag.stencil = #default-flag
  \revert Flag.style
  \textMark "Default flags (Scheme)"
  \testnotes

  \textMark "Symbol: 'mensural (Scheme)"
  \override Flag.style = #'mensural
  \testnotes

  \textMark "Symbol: 'no-flag (Scheme)"
  \override Flag.style = #'no-flag
  \testnotes
}

% New scheme functions: normal-flag, mensural-flag, no-flag
{
  \time 2/4

  \textMark "Function: normal-flag"
  \override Flag.stencil = #normal-flag
  \testnotes

  \textMark "Function: mensural-flag"
  \override Flag.stencil = #mensural-flag
  \testnotes

  \textMark "Function: no-flag"
  \override Flag.stencil = #no-flag
  \testnotes
}
