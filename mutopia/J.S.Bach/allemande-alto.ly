\header{
filename =	 "allemande-alto.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
piece = "Allemande";
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

\include "allemande-urtext.ly";

allemande_alto_global = \melodic{
	\meter 4/4;
	\key bes;
	\partial 16;
	\clef alto;
	\skip 1*11;
	s2. s8.
	\bar ":|:";
	\skip 1*11;
	s2. s4 s8
	\bar ":|";
}

allemande_alto_scripts = \melodic{
}

allemande_alto_staff = \type Staff <
	\$allemande
	\$allemande_alto_global
	\$allemande_alto_scripts
>

\score{
	\$allemande_alto_staff
	\include "scs-paper.ly";
	\midi{ 
		\tempo 4 = 40;
	}
}

