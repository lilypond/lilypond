

\version "1.3.4";

\include "gigue-urtext.ly";

gigue_viola_global = \notes{
	\time 3/8;
	\key f;
	\clef alto;
	\repeat "volta" 2 {
		\partial 8;
		s8
		s4.*31
		s4
		\partial 4;
	} \repeat "volta" 2 {
		% urg
		s16 \partial 8; s16
		s4.*43
		s4
		\partial 4;
	}
}

gigue_viola_scripts = \notes{
}

gigue_viola_staff = \context Staff <
	\notes \transpose c' \$gigue
	\$gigue_viola_global
	\$gigue_viola_scripts
>

\score{
	\$gigue_viola_staff
	\paper{
		\translator{
			\VoiceContext
			beamAutoEnd_8 = #(make-moment 3 4)
			beamAutoEnd_16 = #(make-moment 3 4)
		}
	}
	\midi{ \tempo 4 = 60; }
	\header{
	opus= "" ; 
	piece ="Gigue"; }
}

