\version "2.10.0"
% TODO: huh?  what's this file about?  -gp
% This file shows that Staff grobs can get very deep or high; there is a lot
% of room between the staves. 

\header { texidoc = "
Vertical extents may increased by setting @code{\override VerticalAxisGroup #'minimum-Y-extent}, 
@code{extra-Y-extent}, and @code{Y-extent}. In this example,
@code{Y-extent} is increased.
" }

\score {
    <<
    \new Staff {
      \override Staff.VerticalAxisGroup #'Y-extent = #'(-15.0 . 0.0)
      \clef alto
      a1^"15-deep staff"
    }
    \new Staff {
      \clef alto
      b1
    }
    \new Staff {
      \clef alto
      c1
    }
    \new Staff {
      \clef alto
      \override Staff.VerticalAxisGroup #'Y-extent = #'(-0.0 . 10.0)
      d1^"10-high staff"
    }
  >>
  \layout{
      ragged-right = ##t
  }
}

