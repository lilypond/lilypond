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

\version "1.0.7";

\include "sarabande-urtext.ly";

sarabande_viola_global = \notes{
	\time 3/4;
	\key f;
	\clef alto;
	\skip 2.*12;
	\bar ":|:";
	\skip 2.*16;
	\bar ":|";
}

sarabande_viola_scripts = \notes{
}

sarabande_viola_staff = \type Staff <
	\notes \transpose c' \$sarabande
	\$sarabande_viola_global
	\$sarabande_viola_scripts
>

\score{
	\$sarabande_viola_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 40; }
	\header{ piece = "Sarabande"; }
}

