\score {
  \notes { c4 c4 c8 c8 }
  \paper {
    linewidth = -1.;
    \translator {
      \StaffContext
      \remove Staff_symbol_engraver;
      \consists Pitch_squash_engraver;
      \remove Clef_engraver;
    }
  }
}

