\version "1.7.18"
% covered by refman, I think.  delete or regression.  -gp
\header { texidoc = "DELETE or REGRESSION. "}
\score{
  \context RhythmicStaff {
    \notes\relative c''{ 
      \clef "percussion"
      \time 4/4 
      r4 r g2 | r4 g r2 | g1:32 | r1 |
    }
  }
  \paper{
    \translator{
      \RhythmicStaffContext
      \consists "Clef_engraver"
    }
  }
}
%% new-chords-done %%
