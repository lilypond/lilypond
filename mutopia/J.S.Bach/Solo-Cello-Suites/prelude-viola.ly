


\version "1.3.117";

\include "prelude-urtext.ly";

preludeViolaGlobal =  \notes{
	\time 3/4;
	\key f \major;
	\clef alto;
	\skip 2.*63;
	\bar "|.";
}

preludeViolaScripts =  \notes{
}

preludeViolaStaff =  \context Staff <
	\notes \transpose c' \prelude
	\preludeViolaGlobal
	\preludeViolaScripts
>

\score{
	\preludeViolaStaff
	\paper{ }
	\midi{ \tempo 4 = 40; }
	\header{
	opus= "" ; 
	piece ="Pr\\'elude"; }
}

