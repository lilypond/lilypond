


\version "1.3.110";

\include "prelude-urtext.ly";

prelude_cello_global = \notes{
	\time 3/4;
	\key f \major;
	\clef bass;
	\skip 2.*63;
	\bar "|.";
}

prelude_cello_scripts = \notes{
}

prelude_cello_staff = \context Staff <
	\$prelude
	\$prelude_cello_global
	\$prelude_cello_scripts
>

\score{
	\$prelude_cello_staff
	\paper{ }
	\midi{ \tempo 4 = 40; }
	\header{
	opus= "" ; 
	piece ="Pr\\'elude"; }	
}

