


\version "1.3.117";

\include "gigue-urtext.ly";

gigueCelloGlobal =  \notes{
	\time 3/8;
	\key f \major;
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

gigueCelloScripts =  \notes{
}

gigueCelloStaff =  \context Staff <
	\gigue
	\gigueCelloGlobal
	\gigueCelloScripts
>

\score{
	\gigueCelloStaff
	\paper{
		\translator{
			\VoiceContext
			autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 3 4)
			autoBeamSettings \override #'(end 1 16 * *) = #(make-moment 3 4)
		}
	}
	\midi{ \tempo 4 = 60; }
	\header{
	opus= "" ; 
piece = "Gigue"; }
}

