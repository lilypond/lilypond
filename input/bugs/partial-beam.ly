\version "1.3.148"
%docme -- geen id, lijkt ok?

\include "paper20.ly"
Sopnotes = \notes {
	\time 4/4
	\key g \major
	\clef treble
	\partial 8 * 3
	e'8 f'8 g'8 |
	d'8 d'16 d'16 e'8. e'16 g'8 g'8 f'8 f'8
}

\score {
	\notes
	<
		\context Staff="sop"
		<
		    \Sopnotes
		>
	>
	\paper {
                \translator {
                 \StaffContext 
                        autoBeamSettings \override #'(end * * * * ) = #(make-moment 1 8)
		}
	}
}
