


\version "1.3.42";

\include "gigue-urtext.ly";

gigue_cello_global = \notes{
	\time 3/8;
	\key f;
	\clef bass;
	\repeat "volta" 2 {
		\partial 8;
		s8
		s4.*31
		s4
		%\partial 4;
	} \repeat "volta" 2 {
		% urg
		%s16 \partial 8; s16
		s8
		s4.*43
		s4
		%\partial 4;
	}
}

gigue_cello_scripts = \notes{
}

gigue_cello_staff = \context Staff <
	\$gigue
	\$gigue_cello_global
	\$gigue_cello_scripts
>

\score{
	\$gigue_cello_staff
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

