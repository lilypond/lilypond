\version "2.3.4"

\header {
texidoc = "Note head lines (e.g. glissando)
run between centers of the note heads."
}

su = { \change Staff = up}
sd = { \change Staff = down}

\score {
    \context PianoStaff
         <<
	\context Staff = up   {
            \set PianoStaff.connectArpeggios = ##t
	    \showStaffSwitch
	    \clef F
	    c4 d \sd b a g8 f16 e32 d \su g2 \glissando a,4 \sd \break a2. \su g4 \glissando f1
	}
	\context Staff = down {
	    \clef F s1*4
	}
    >>
\paper {
    linewidth = 8.0 \cm
}
}


