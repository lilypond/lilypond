\header{
filename =	 "allemande-cello.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
%piece = "Allemande";
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

\include "allemande-urtext.ly";

allemande_cello_global = \notes{
	\time 4/4;
	\key f;
	\clef bass;
	\repeat 2 {
		\partial 16;
		s16
		s1*11 |
		s2 s4 s8
		\partial 16*15;
		s16
	} \repeat 2 {
		% urg
		s32 \partial 16; s32
		s1*11
		s16*15
		\partial 16*15;
	}
}

allemande_cello_scripts = \notes{
}

allemande_cello_staff = \context Staff <
	\$allemande
	\$allemande_cello_global
	\$allemande_cello_scripts
>

\score{
   	\$allemande_cello_staff
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 3.0;
	}
	\midi{ \tempo 4 = 45; }
	\header{ piece = "Allemande"; }
}
