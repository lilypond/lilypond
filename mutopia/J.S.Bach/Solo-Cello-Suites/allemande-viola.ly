
\version "1.3.4";

\include "allemande-urtext.ly";

allemande_viola_global = \notes{
	\time 4/4;
	\key f;
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

allemande_viola_scripts = \notes{
}

allemande_viola_staff = \context Staff <
	\notes \transpose c'' \$allemande
	\$allemande_viola_global
	\$allemande_viola_scripts
>

\score{
	\$allemande_viola_staff
	\paper{ }
	\midi{ \tempo 4 = 45; }
	\header{ piece = "Allemande";
	  opus = "";
	}
}

