

\version "1.3.117";

\include "allemande-urtext.ly";

allemandeCelloGlobal =  \notes{
	\time 4/4;
	\key f \major;
	\clef bass;
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

allemandeCelloScripts =  \notes{
}

allemandeCelloStaff =  \context Staff <
	\allemande
	\allemandeCelloGlobal
	\allemandeCelloScripts
>

\score{
   	\allemandeCelloStaff
	\paper{ }
	\midi{ \tempo 4 = 45; }
	\header{ piece = "Allemande";
	  opus = "";

	  }
}
