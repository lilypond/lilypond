\header {

texidoc = "A clef can be folded below notes in a different staff, if
there is space enough. With Paper_column molecule callbacks we can
show where columns are in the score."
}

\score { \notes \relative c'' <
	\context Staff = SA  { c4 c4 c4 \bar "|." }
	\context Staff = SB { \clef bass c,2 \clef treble  c'2 }
	>

	\paper { linewidth = -1.

	\translator { \ScoreContext
	  NonMusicalPaperColumn \override #'molecule-callback = #Paper_column::brew_molecule
	  PaperColumn \override #'molecule-callback = #Paper_column::brew_molecule	  
	  NonMusicalPaperColumn \override #'font-family = #'roman
	  PaperColumn \override #'font-family = #'roman	  

	}
	}}

