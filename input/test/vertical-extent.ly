
\header { texidoc = "vertical extents may be overriden by
VerticalExtent, VerticalExtent, VerticalExtent. These are
normal property values, and are written itno the grob when the
associated context finishes, so using it in \property works.

 "; }
\version "1.3.145";

\score {
  \notes  <
    \context Staff = upper {
      \property Staff.VerticalExtent = #'(-15.0 . 0.0)
      \clef alto;
      c1 \break c1 
    }
    \context Staff = lower {
      \property Staff.VerticalExtent = #'(-0.0 . 15.0)
      \clef alto;
      g1 \break g1 
    }
  >
  \paper{
    interscoreline = 13.0\mm;
    interscorelinefill = 0;
    \translator{\ScoreContext \remove "Bar_number_engraver";}
    \translator{\StaffContext MinimumVerticalExtent = #'(-2.0 . 2.0)}
  }
}
