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

\version "1.0.7";

\include "courante-urtext.ly";

courante_viola_global = \notes{
	\time 3/4;
	\key f;
	\partial 16;
	\clef alto;
	\skip 2.*15;
	s2 s8.
	\bar ":|:";
	\skip 2.*15;
	s2 s8.
	\bar ":|";
}

courante_viola_scripts = \notes{
}

courante_viola_staff = \type Staff <
	\notes \transpose c' \$courante
	\$courante_viola_global
	\$courante_viola_scripts
>

\score{
	\$courante_viola_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 55; }
	\header{ piece = "Courante"; }
}

