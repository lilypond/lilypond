


\version "1.3.117";

\include "prelude-urtext.ly";

preludeCelloGlobal =  \notes{
	\time 3/4;
	\key f \major;
	\clef bass;
	\skip 2.*63;
	\bar "|.";
}

preludeCelloScripts =  \notes{
}

preludeCelloStaff =  \context Staff <
	\prelude
	\preludeCelloGlobal
	\preludeCelloScripts
>

\score{
	\preludeCelloStaff
	\paper{ }
	\midi{ \tempo 4 = 40; }
	\header{
	opus= "" ; 
	piece ="Pr\\'elude"; }	
}

