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

\version "1.0.14";

\include "menuetto-urtext.ly";

menuetto_i_viola_global = \notes{
	\time 3/4;
	\key f;
	\clef alto;
	\repeat 2 {
		\skip 2.*8;
		\clef violin;
		\skip 2.*1;
	} \repeat 2 {
		\slurdotted
		\skip 2.*3;
		\clef alto;
		\skip 2.*11;
		s2._"Fine"
	}
}

menuetto_i_viola_scripts = \notes{
	\type Voice=i
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
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 7.0;
		\translator{
			\VoiceContext
			beamAutoEnd_8 = "3/4";
		}
	}
	\midi{ \tempo 4 = 110; }
	\header{ piece = "Menuetto I"; }
}

menuetto_ii_viola_global = \notes{
	\time 3/4;
	\key D;
	\clef alto;
	\repeat 2 {
		\skip 2.*8;
	} \repeat 2 {
		\skip 2.*1;
		\slurdotted
		\skip 2.*14;
		s2._"Menuetto I da Capo"
	}
}

menuetto_ii_viola_staff = \type Staff <
	\notes \transpose c' \$menuetto_ii
	\$menuetto_ii_viola_global
%	\$menuetto_ii_viola_scripts
>

\score{
	\$menuetto_ii_viola_staff
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 7.0;
		\translator{
			\VoiceContext
			beamAutoEnd_8 = "3/4";
		}
	}
	\midi{ \tempo 4 = 130; }
	\header{ piece = "Menuetto II"; }
}

