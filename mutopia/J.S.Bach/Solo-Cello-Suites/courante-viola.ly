\header{
filename =	 "courante-viola.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
% piece = "Courante";
% opus =	 "BWV 1008";
opus =	 "";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

%{
 Tested Features:breaking algorithm, chords, multivoice, accents, 
 dotted slurs
%}

\version "1.0.16";

\include "courante-urtext.ly";

courante_viola_global = \notes{
	\time 3/4;
	\key f;
	\clef alto;
	\repeat 2 {
		\partial 16;
		s16
		s2.*15
		% hmm
		s2 s8
		\partial 16*11;
		s16
	} \repeat 2 {
		% urg
		s32 \partial 16; s32
		s2.*15
		s16*11
		\partial 16*11;
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
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 4.0;
	}
	\midi{ \tempo 4 = 55; }
	\header{ piece = "Courante"; }
}

