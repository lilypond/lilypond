\header{
filename =	 "sarabande-cello.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
% piece = "Sarabande";
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

\version "1.1.66";

\include "sarabande-urtext.ly";

sarabande_cello_global = \notes{
	\time 3/4;
	\key f;
	\clef bass;
	\repeat "semi" 2 {
		s2.*12
	} \repeat "semi" 2 {
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
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 5.0;
	}
	\midi{ \tempo 4 = 40; }
	\header{ piece = "Sarabande"; }
}

