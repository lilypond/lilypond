


\version "1.2.0";

\include "courante-urtext.ly";

courante_viola_global = \notes{
	\time 3/4;
	\key f;
	\clef alto;
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

courante_viola_scripts = \notes{
}

courante_viola_staff = \context Staff <
	\notes \transpose c' \$courante
	\$courante_viola_global
	\$courante_viola_scripts
>

\score{
	\$courante_viola_staff
	\paper{ }
	\midi{ \tempo 4 = 55; }
	\header{
		opus= "" ; 
		piece ="Courante";
	}
}

