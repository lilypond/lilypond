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

\version "1.0.1";

\include "sarabande-urtext.ly";

sarabande_cello_global = \melodic{
	\time 3/4;
	\key f;
	\clef bass;
	\skip 2.*12;
	\bar ":|:";
	\skip 2.*16;
	\bar ":|";
}

sarabande_cello_scripts = \melodic{
}

sarabande_cello_staff = \type Staff <
	\$sarabande
	\$sarabande_cello_global
	\$sarabande_cello_scripts
>

\score{
	\$sarabande_cello_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 40; }
	\header{ piece = "Sarabande"; }
}

