\version "2.11.51"
% possible rename to staff-something.  -gp

\header{ texidoc = "@cindex Staff Remove
The printing of the staff lines may be suppressed by removing the 
corresponding engraver.
"
}

\layout {
    ragged-right = ##t
}

\relative \new Staff \with {
	\remove Staff_symbol_engraver
	\consists Pitch_squash_engraver
	\remove Clef_engraver
    } {
	c4 d4 e8 d8
    }


