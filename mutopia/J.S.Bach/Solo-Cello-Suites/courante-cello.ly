
\version "1.3.117";

\include "courante-urtext.ly";

couranteCelloGlobal =  \notes{
	\time 3/4;
	\key f \major;
	\clef bass;
	\repeat "volta" 2 {
		\partial 16;
		s16
		s2.*15
		s2 s8.
	} \repeat "volta" 2 {
		s16
		s2.*15
		s16*11
	}
}

couranteCelloScripts =  \notes{
}

couranteCelloStaff =  \context Staff <
	\courante
	\couranteCelloGlobal
	\couranteCelloScripts
>

\score{
	\couranteCelloStaff
	\paper {}
	\midi{ \tempo 4 = 55; }
	\header{ piece = "Courante";
		  opus = "";
		  }
}

