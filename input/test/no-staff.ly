\version "1.9.1"
% possible rename to staff-something.  -gp

\header{ texidoc = "@cindex Staff Remove
You can stop LilyPond from printing the staff by removing the engraver. "
}

\score {
  \notes { c4 d4 e8 d8 }
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


