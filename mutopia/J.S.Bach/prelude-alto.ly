\header{
filename =	 "prelude-alto.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
piece = "Pr\\'elude";		% duh
% opus =	 "BWV 1008";
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

\include "prelude-urtext.ly";

prelude_alto_global = \melodic{
	\meter 3/4;
	\key bes;
	\clef alto;
	\skip 2.*63;
	\bar "|.";
}

prelude_alto_scripts = \melodic{
}

prelude_alto_staff = \type Staff <
	\$prelude
	\$prelude_alto_global
	\$prelude_alto_scripts
>

\score{
	\$prelude_alto_staff
	\include "scs-paper.ly";
	\midi{ 
		\tempo 4 = 40;
	}
}

