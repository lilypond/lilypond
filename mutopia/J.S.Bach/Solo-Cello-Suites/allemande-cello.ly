\header{
filename =	 "allemande-cello.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
%piece = "Allemande";
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

\include "allemande-urtext.ly";

allemande_cello_global = \notes{
	\time 4/4;
	\key f;
	\partial 16;
	\clef bass;
	\skip 1*11;
	s2. s8.
	\bar ":|:";
	\skip 1*11;
	s2. s4 s8
	\bar ":|";
}

allemande_cello_scripts = \notes{
}

allemande_cello_staff = \type Staff <
	\$allemande
	\$allemande_cello_global
	\$allemande_cello_scripts
>

\score{
	\$allemande_cello_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 45; }
	\header{ piece = "Allemande"; }
}

