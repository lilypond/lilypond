


\version "1.3.93";

\include "sarabande-urtext.ly";

sarabande_cello_global = \notes{
	\time 3/4;
	\key f \major;
	\clef bass;
	\repeat "volta" 2 {
		s2.*12
	} \repeat "volta" 2 {
		s2.*16
	}
}

sarabande_cello_scripts = \notes{
}

sarabande_cello_staff = \context Staff <
	\$sarabande
	\$sarabande_cello_global
	\$sarabande_cello_scripts
>

\score{
	\$sarabande_cello_staff
	\paper{ }
	\midi{ \tempo 4 = 40; }
	\header{
	opus= "" ; 
	piece ="Sarabande"; }
}

