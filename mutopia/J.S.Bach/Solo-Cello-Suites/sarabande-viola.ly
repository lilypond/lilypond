\version "1.3.117";

\include "sarabande-urtext.ly";

sarabandeViolaGlobal =  \notes{
	\time 3/4;
	\key f \major;
	\clef alto;
	\repeat "volta" 2 {
		s2.*12
	} \repeat "volta" 2 {
		s2.*16
	}
}

sarabandeViolaScripts =  \notes{
}

sarabandeViolaStaff =  \context Staff <
	\notes \transpose c' \sarabande
	\sarabandeViolaGlobal
	\sarabandeViolaScripts
>

\score{
	\sarabandeViolaStaff
	\paper{	}
	\midi{ \tempo 4 = 40; }
	\header{
	opus= "" ; 
	piece ="Sarabande"; }
}

