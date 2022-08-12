\version "2.23.12"

\header {
  texidoc = "The @code{\\staffHighlight} command highlights a musical passage.
A highlight is terminated by @code{\\stopStaffHighlight}, by the start
of another highlight, or by the end of the music."
}

{
  \staffHighlight darkturquoise
  c'8 g' d'' g' d''2
  % Explicit stop.
  \stopStaffHighlight
  c'1
  \staffHighlight darkturquoise
  1
  % Implicit stop.
  \staffHighlight beige
  1
  % Explicit stop can also be used.
  \stopStaffHighlight
  \staffHighlight limegreen
  1
  % Implicit stop at end of piece.
}
#(debug-enable 'backtrace)
\new Staff \with {
  \consists Staff_highlight_engraver
}
{
 % \override Staff.StaffHighlight.shorten-pair = #'(0.3 . 0)
%  \override Staff.TimeSignature.space-alist.first-note = #'(fixed-space . -1)
  \staffHighlight darkturquoise
  \repeat unfold 16 g'16
  % Explicit stop can also be used at end of piece.
  \stopStaffHighlight
}
