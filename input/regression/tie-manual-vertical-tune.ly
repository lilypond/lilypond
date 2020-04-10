\version "2.19.21"

\header {
  texidoc = "If using exact values (this is, either integers or
rational values like @samp{(/ 4 5)}), @code{staff-position} is used to
vertically tune the tie for staff line avoidance.  If using an inexact
value like a floating point number, it is taken as the vertical
location."
}

\relative {
  \override Tie.staff-position = #3
  d''4 ~
  \override Tie.staff-position = #3.0
  d ~
  d
}
