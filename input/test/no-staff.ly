\version "1.7.16"

\score {
  \notes { c4 c4 c8 c8 }
  \paper {
    raggedright = ##t
    \translator {
      \StaffContext
      \remove Staff_symbol_engraver
      \consists Pitch_squash_engraver
      \remove Clef_engraver
    }
  }
}

%% new-chords-done %%
