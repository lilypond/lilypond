
\score{
  \context RhythmicStaff {
    \notes\relative c''{ 
      \property RhythmicStaff.clefGlyph = #"clefs-percussion"
	\property RhythmicStaff.clefPosition = #0
      \time 4/4; 
      r4 r g2 | r4 g r2 | g1:32 | r1 |
    }
  }
  \paper{
    \translator{
      \RhythmicStaffContext
      \consists "Clef_engraver";
    }
  }
}
