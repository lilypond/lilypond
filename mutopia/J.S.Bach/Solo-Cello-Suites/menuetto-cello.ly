\header{
filename =	 "menuetto-cello.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
%piece = "Menuetto I";
source =	 "?";
% opus =	 "BWV 1008 no. 5";
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

\include "menuetto-urtext.ly";

menuetto_i_cello_global = \notes{
	\time 3/4;
	\key f;
	\clef bass;
	\skip 2.*8;
	\bar ":|:";
	\skip 2.*1;
	\slurdotted
	\skip 2.*14;
	s2._"Fine"
	\bar ":|";
}

menuetto_i_cello_scripts = \notes{
	s2.
	s8^"~"^1_2_4 s8 s4 s^4
	s4^0_1 s_4 s
	s2.*5
	s2^3 s4
	s4 s8_1 s s4
	s2.
	s2 s8^4 s
	s2.
	s8 s^2 s^4
	s_2 s s s_0 s_4 s_1
	s2.*2
	s4^3_1
	s^1_3 s4
	s2.
	s4_2 s2
	s8^2_3 s s s^1 s4^1
}

menuetto_i_cello_staff = \type Staff <
	\$menuetto_i
	\$menuetto_i_cello_global
%	\$menuetto_i_cello_scripts
>

\score{
	\$menuetto_i_cello_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 110; }
	\header{ piece = "Menuetto I"; }
}

menuetto_ii_cello_global = \notes{
	\time 3/4;
	\key D;
	\clef bass;
	\skip 2.*8;
	\bar ":|:";
	\skip 2.*1;
	\slurdotted
	\skip 2.*14;
	s2._"Menuetto I da Capo"
	\bar ":|";
}

menuetto_ii_cello_staff = \type Staff <
	\$menuetto_ii
	\$menuetto_ii_cello_global
%	\$menuetto_ii_cello_scripts
>

\score{
	\$menuetto_ii_cello_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 130; }
	\header{ piece = "Menuetto II"; }
}

