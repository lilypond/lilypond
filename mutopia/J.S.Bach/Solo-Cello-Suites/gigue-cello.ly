\header{
filename =	 "gigue-cello.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
% piece = "Gigue";
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

\include "gigue-urtext.ly";

gigue_cello_global = \notes{
	\time 3/8;
	\key f;
	\clef bass;
	\partial 8;
	\skip 4.*31;
	s4
	\bar ":|:";
	\skip 4.*44;
	s4
	\bar ":|";
}

gigue_cello_scripts = \notes{
}

gigue_cello_staff = \type Staff <
	\$gigue
	\$gigue_cello_global
	\$gigue_cello_scripts
>

\score{
	\$gigue_cello_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 60; }
	\header{ piece = "Gigue"; }
}

