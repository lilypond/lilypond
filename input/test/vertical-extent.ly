
\score {
  \notes  <
    \context Staff = upper {
      \property Staff.StaffVerticalExtent = #'(-15.0 . 0.0)
      \clef alto;
      c1 \break c1 
    }
    \context Staff = lower {
      \property Staff.StaffVerticalExtent = #'(-0.0 . 15.0)
      \clef alto;
      g1 \break g1 
    }
  >
  \paper{
    interscoreline = 3.0\mm;
    \translator{\ScoreContext \remove "Bar_number_engraver";}
    \translator{\StaffContext StaffMinimumVerticalExtent = #'(-2.0 . 2.0)}
  }
}
