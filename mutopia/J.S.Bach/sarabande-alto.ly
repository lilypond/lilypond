\header{
filename =	 "sarabande-alto.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
piece = "Sarabande";
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

\version "0.1.14";

urg_urg = \melodic {
      \octave relative;
      \octave c';
}

\include "sarabande-urtext.ly";

sarabande_alto_global = \melodic{
	\meter 3/4;
	\key bes;
	\clef alto;
	\skip 2.*12;
	\bar ":|:";
	\skip 2.*16;
	\bar ":|";
}

sarabande_alto_scripts = \melodic{
}

sarabande_alto_staff = \type Staff <
	\$sarabande
	\$sarabande_alto_global
	\$sarabande_alto_scripts
>

\score{
	\$sarabande_alto_staff
	\include "scs-paper.ly";
	\midi{ 
		\tempo 4 = 40;
	}
}

