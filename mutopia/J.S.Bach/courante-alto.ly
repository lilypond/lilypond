\header{
filename =	 "courante-alto.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
piece = "Courante";
% opus =	 "BWV 1008";
opus =	 "";
composer =	 "Johann Sebastian Bach(1685-1750)";
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

\include "courante-urtext.ly";

courante_alto_global = \melodic{
	\meter 3/4;
	\key bes;
	\partial 16;
	\clef alto;
	\skip 2.*15;
	s2 s8.
	\bar ":|:";
	\skip 2.*15;
	s2 s8
	\bar ":|";
}

courante_alto_scripts = \melodic{
}

courante_alto_staff = \type Staff <
	\$courante
	\$courante_alto_global
	\$courante_alto_scripts
>

\score{
	\$courante_alto_staff
	\include "scs-paper.ly";
	\midi{ 
		\tempo 4 = 40;
	}
}

