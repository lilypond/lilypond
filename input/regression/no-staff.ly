\version "2.19.21"
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
	\remove "Staff_symbol_engraver"
	\consists "Pitch_squash_engraver"
	\remove "Clef_engraver"
    } {
	c'4 d4 e8 d8
    }


