\header{
filename =	 "menuetto-viola.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
% piece = "Menuetto I";
description =	 "Transcribed for Viola";
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

menuetto_i_viola_global = \notes{
	\time 3/4;
	\key f;
	\clef alto;
	\skip 2.*8;
	\bar ":|:";
	\clef violin;
	\skip 2.*1;
	\slurdotted
	\skip 2.*3;
	\clef alto;
	\skip 2.*11;
	s2._"Fine"
	\bar ":|";
}

menuetto_i_viola_scripts = \notes{
	s2.
	s8^"~"^1_2_4 s8*5
	s2.*5
	s4 s-\upbow s-\downbow
	s2.-\upbow
	s2.*5
	s2 s4-\upbow
	s4-\downbow s2
	s2.*1
	s2^0 s4
	s2.*1
	s4-\downbow s4-\upbow
}

menuetto_i_viola_staff = \type Staff <
	\notes \transpose c' \$menuetto_i
	\$menuetto_i_viola_global
%	\$menuetto_i_viola_scripts
>

\score{
	\$menuetto_i_viola_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 110; }
	\header{ piece = "Menuetto I"; }
}

menuetto_ii_viola_global = \notes{
	\time 3/4;
	\key D;
	\clef alto;
	\skip 2.*8;
	\bar ":|:";
	\skip 2.*1;
	\slurdotted
	\skip 2.*14;
	s2._"Menuetto I da Capo"
	\bar ":|";
}

menuetto_ii_viola_staff = \type Staff <
	\notes \transpose c' \$menuetto_ii
	\$menuetto_ii_viola_global
%	\$menuetto_ii_viola_scripts
>

\score{
	\$menuetto_ii_viola_staff
	\include "scs-paper.ly";
	\midi{ \tempo 4 = 130; }
	\header{ piece = "Menuetto II"; }
}

