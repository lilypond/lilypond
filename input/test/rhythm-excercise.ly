\version "2.3.2"
\header { texidoc = "@cindex Rhythm Exercise
Rythmic exercises may be produced by removing the @code{Clef} engraver,
putting all notes to the same pitch and using transparent staff lines.
"
}

\score { \notes { c4 c4 c8[ c8]  c2 c2 }

	 \paper {
	     \context { \Staff
			   \override StaffSymbol #'transparent = ##t
			   \consists Pitch_squash_engraver
			   \remove Clef_engraver
		       }
	     raggedright= ##t	     
	 }
}

