


\version "1.3.117";

\include "courante-urtext.ly";

couranteViolaGlobal =  \notes{
	\time 3/4;
	\key f \major;
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

couranteViolaScripts =  \notes{
}

couranteViolaStaff =  \context Staff <
	\notes \transpose c' \courante
	\couranteViolaGlobal
	\couranteViolaScripts
>

\score{
	\couranteViolaStaff
	\paper{ }
	\midi{ \tempo 4 = 55; }
	\header{
		opus= "" ; 
		piece ="Courante";
	}
}

