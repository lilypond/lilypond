\version "1.3.93";

\include "sarabande-urtext.ly";

sarabande_viola_global = \notes{
	\time 3/4;
	\key f \major;
	\clef alto;
	\repeat "volta" 2 {
		s2.*12
	} \repeat "volta" 2 {
		s2.*16
	}
}

sarabande_viola_scripts = \notes{
}

sarabande_viola_staff = \context Staff <
	\notes \transpose c' \$sarabande
	\$sarabande_viola_global
	\$sarabande_viola_scripts
>

\score{
	\$sarabande_viola_staff
	\paper{	}
	\midi{ \tempo 4 = 40; }
	\header{
	opus= "" ; 
	piece ="Sarabande"; }
}

