\version "2.16.0"
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
  \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \time 2/4

  \mark "Default flags (C++)"
  \testnotes

  \mark "Symbol: 'mensural (C++)"
  \override Flag #'style = #'mensural
  \testnotes

  \mark "Symbol: 'no-flag (C++)"
  \override Flag #'style = #'no-flag
  \testnotes
}

% The same, but using the Scheme implementation of default-flag
{
  \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \time 2/4

  \override Flag #'stencil = #default-flag
  \revert Flag #'style
  \mark "Default flags (Scheme)"
  \testnotes

  \mark "Symbol: 'mensural (Scheme)"
  \override Flag #'style = #'mensural
  \testnotes

  \mark "Symbol: 'no-flag (Scheme)"
  \override Flag #'style = #'no-flag
  \testnotes
}

% New scheme functions: normal-flag, mensural-flag, no-flag
{
  \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \time 2/4

  \mark "Function: normal-flag"
  \override Flag #'stencil = #normal-flag
  \testnotes

  \mark "Function: mensural-flag"
  \override Flag #'stencil = #mensural-flag
  \testnotes

  \mark "Function: no-flag"
  \override Flag #'stencil = #no-flag
  \testnotes
}
