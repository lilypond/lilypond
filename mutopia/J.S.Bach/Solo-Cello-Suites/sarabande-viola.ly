\header{
filename =	 "sarabande-viola.ly";
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

sarabande_viola_global = \notes{
	\time 3/4;
	\key f;
	\clef alto;
	\repeat "semi" 2 {
		s2.*12
	} \repeat "semi" 2 {
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
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 5.0;
	}
	\midi{ \tempo 4 = 40; }
	\header{ piece = "Sarabande"; }
}

