\version "2.1.22"
% TODO: huh?  what's this file about?  -gp

\header { texidoc = "
Vertical extents may be overriden by
minimumVerticalExtent, extraVerticalExtent, and  verticalExtent. These are
normal property values, and are written into the grob when the
associated context finishes, so using it in \property works.
" }

\score {
  \notes  <<
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
  \paper{
      raggedright = ##t
  }
}

