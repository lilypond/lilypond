
\version "1.3.117";

\include "allemande-urtext.ly";

allemandeViolaGlobal =  \notes{
	\time 4/4;
	\key f \major;
	\clef alto;
	\repeat "volta" 2 {
		\partial 16;
		s16
		s1*11 |
		s2 s4 s8.
	} \repeat "volta" 2 {
		s16
		s1*11
		s16*15
	}
}

allemandeViolaScripts =  \notes{
}

allemandeViolaStaff =  \context Staff <
	\notes \transpose c'' \allemande
	\allemandeViolaGlobal
	\allemandeViolaScripts
>

\score{
	\allemandeViolaStaff
	\paper{ }
	\midi{ \tempo 4 = 45; }
	\header{ piece = "Allemande";
	  opus = "";
	}
}

