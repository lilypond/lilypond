


\version "1.3.117";

\include "sarabande-urtext.ly";

sarabandeCelloGlobal =  \notes{
	\time 3/4;
	\key f \major;
	\clef bass;
	\repeat "volta" 2 {
		s2.*12
	} \repeat "volta" 2 {
		s2.*16
	}
}

sarabandeCelloScripts =  \notes{
}

sarabandeCelloStaff =  \context Staff <
	\sarabande
	\sarabandeCelloGlobal
	\sarabandeCelloScripts
>

\score{
	\sarabandeCelloStaff
	\paper{ }
	\midi{ \tempo 4 = 40; }
	\header{
	opus= "" ; 
	piece ="Sarabande"; }
}

