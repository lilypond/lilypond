\version "1.7.18"
% TODO: huh?  what's this file about?  -gp

\header { texidoc = "
Vertical extents may be overriden by
minimumVerticalExtent, extraVerticalExtent, and  verticalExtent. These are
normal property values, and are written into the grob when the
associated context finishes, so using it in \property works.
" }

\score {
  \notes  <
    \context Staff = upper {
      \property Staff.verticalExtent = #'(-15.0 . 0.0)
      \clef alto
      c1 \break c1 
    }
    \context Staff = lower {
      \property Staff.verticalExtent = #'(-0.0 . 15.0)
      \clef alto
      g1 \break g1 
    }
  >
  \paper{
    interscoreline = 13.0\mm
    interscorelinefill = 0
    \translator{\ScoreContext \remove "Bar_number_engraver"}
    \translator{\StaffContext minimumVerticalExtent = #'(-2.0 . 2.0)}
  }
}

