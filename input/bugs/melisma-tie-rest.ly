\header{
    texidoc = "Lyric extenders that start because of an autmatic tie melisma,
extend during a subsequent rest, and cannot be forced to stop."
}
\score{
    <
	\addlyrics
	\context Staff = soprano {
	    \property Staff.automaticMelismata = ##t
	    \context Voice=soprano\notes\relative c'' {
		c4 ~ c r2 r1 c
		c4 ~ c
		\melismaEnd
		\property Staff.tieMelismaBusy = ##f
		r2 r1 c
	    }
	}
	\context Lyrics = "soprano-1" \lyrics {
	    hey __ stop?
	    hey __ STOP!!!
	}
    >
}

