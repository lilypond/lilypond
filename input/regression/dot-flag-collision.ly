\version "1.5.68"
\header {
    texidoc = "Dots move to the right when a collision with the (up)flag happens"
}

\score {
    \notes\relative c''
    {
	\time 8/4
	\property Staff.autoBeaming = ##f
	a,16.
	g''16.
	a,4. a8 a8. a16. c,16. g16.
    }
    \paper { linewidth = -1. }
}

