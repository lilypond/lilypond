

\version "1.3.110";

\include "allemande-urtext.ly";

allemande_cello_global = \notes{
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

allemande_cello_scripts = \notes{
}

allemande_cello_staff = \context Staff <
	\$allemande
	\$allemande_cello_global
	\$allemande_cello_scripts
>

\score{
   	\$allemande_cello_staff
	\paper{ }
	\midi{ \tempo 4 = 45; }
	\header{ piece = "Allemande";
	  opus = "";

	  }
}
