\header{
filename =	 "prelude-alto.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
piece = "Pr\\'elude";		% duh
opus =	 "BWV 1008";
% opus =	 "";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

%{
 Tested Features:breaking algorithm, chords, multivoice, accents, 
 dotted slurs
%}

\version "1.0.0";

\include "prelude-urtext.ly";

prelude_alto_global = \melodic{
	\time 3/4;
	\key f;
	\clef alto;
	\skip 2.*63;
	\bar "|.";
}

prelude_alto_scripts = \melodic{
}

prelude_alto_staff = \type Staff <
	\melodic \transpose c' \$prelude
	\$prelude_alto_global
	\$prelude_alto_scripts
>

\score{
	\$prelude_alto_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 40; }
}

