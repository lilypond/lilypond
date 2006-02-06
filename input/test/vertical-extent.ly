\version "2.7.32"
% TODO: huh?  what's this file about?  -gp

\header { texidoc = "
Vertical extents may increased by setting @code{\override VerticalAxisGroup #'minimum-Y-extent}, 
@code{extraVerticalExtent}, and @code{verticalExtent}. In this example,
@code{verticalExtent} is increased.
" }

\score {
    <<
    \new Staff {
      \set Staff.verticalExtent = #'(-15.0 . 0.0)
      \clef alto
      c1
    }
    \new Staff {
      \set Staff.verticalExtent = #'(-0.0 . 15.0)
      \clef alto
      g1
    }
  >>
  \layout{
      ragged-right = ##t
  }
}

