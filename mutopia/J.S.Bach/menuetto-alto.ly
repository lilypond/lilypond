\header{
filename =	 "menuetto-alto.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
piece = "Menuetto I";
description =	 "Transcribed for Alto";
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

\version "0.1.14";

urg_urg = \melodic {
      \octave relative;
      \octave c';
}

\include "menuetto-urtext.ly";

menuetto_i_alto_global = \melodic{
	\meter 3/4;
	\key bes;
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

menuetto_i_alto_scripts = \melodic{
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

menuetto_i_alto_staff = \type Staff <
	\$menuetto_i
	\$menuetto_i_alto_global
%	\$menuetto_i_alto_scripts
>

\score{
	\$menuetto_i_alto_staff
	\include "scs-paper.ly";
	\midi{ 
		\tempo 4 = 120;
	}
}

menuetto_ii_alto_global = \melodic{
	\meter 3/4;
	\key fis cis;
	\clef alto;
	\skip 2.*8;
	\bar ":|:";
	\skip 2.*1;
	\slurdotted
	\skip 2.*14;
	s2._"Menuetto I da Capo"
	\bar ":|";
}

menuetto_ii_alto_staff = \type Staff <
	\$menuetto_ii
	\$menuetto_ii_alto_global
%	\$menuetto_ii_alto_scripts
>

\header{
piece = "Menuetto II";
}
\score{
	\$menuetto_ii_alto_staff
	\include "scs-paper.ly";
	\midi{ 
		\tempo 4 = 120;
	}
}

