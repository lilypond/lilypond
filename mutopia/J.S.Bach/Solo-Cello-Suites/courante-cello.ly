
\version "1.3.59";

\include "courante-urtext.ly";

courante_cello_global = \notes{
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

courante_cello_scripts = \notes{
}

courante_cello_staff = \context Staff <
	\$courante
	\$courante_cello_global
	\$courante_cello_scripts
>

\score{
	\$courante_cello_staff
	\paper {}
	\midi{ \tempo 4 = 55; }
	\header{ piece = "Courante";
		  opus = "";
		  }
}

