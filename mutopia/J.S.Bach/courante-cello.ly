\header{
filename =	 "courante-cello.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
piece = "Courante";
% opus =	 "BWV 1008";
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
      \octave c;
}

\include "courante-urtext.ly";

courante_cello_global = \melodic{
	\meter 3/4;
	\key bes;
	\partial 16;
	\clef alto;
	\skip 2.*16;
	\bar ":|:";
	\skip 2.*16;
	\bar ":|";
}

courante_cello_scripts = \melodic{
}

courante_cello_staff = \type Staff <
	\$courante
	\$courante_cello_global
	\$courante_cello_scripts
>

\score{
	\$courante_cello_staff
	\include "scs-paper.ly";
	\midi{ 
		\tempo 4 = 40;
	}
}

