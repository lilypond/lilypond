\version "2.1.23"
\header { texidoc = "@cindex Rhythm Exercise
This example shows a way to generate rhythm exercises with
LilyPond (e.g. no staff but retaining the barlines). "
}

\score { \notes { c4 c4 c8[ c8]  c2 c2 }

	 \paper {
	     \translator { \StaffContext
			   \override StaffSymbol #'transparent = ##t
			   \consists Pitch_squash_engraver
			   \remove Clef_engraver
		       }
	     raggedright= ##t	     
	 }
}

