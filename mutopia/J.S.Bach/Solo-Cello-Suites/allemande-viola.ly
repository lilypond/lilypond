\header{
filename =	 "allemande-viola.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
% piece = "Allemande";
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

\version "1.0.14";

\include "allemande-urtext.ly";

allemande_viola_global = \notes{
	\time 4/4;
	\key f;
	\clef alto;
	\repeat 2 {
		\partial 16;
		s16
		s1*11 |
		s2 s4 s8
		\partial 16*15;
		s16
	} \repeat 2 {
		% urg
		s32 \partial 16; s32
		s1*11
		s16*15
		\partial 16*15;
	}
}

allemande_viola_scripts = \notes{
}

allemande_viola_staff = \type Staff <
	\notes \transpose c'' \$allemande
	\$allemande_viola_global
	\$allemande_viola_scripts
>

\score{
	\$allemande_viola_staff
%	\include "scs-paper.ly";
	\paper{
	        linewidth = 180.\mm;
		\translator{
			\VoiceContext
			% add experimental auto-beaming
			\consists Auto_beam_engraver;
			beamAuto = 1.;
			beamAutoEnd8 = "1/4";
			beamAutoEnd16 = "1/4";
			beamAutoEnd32 = "1/4";
		}
	}
	\midi{ \tempo 4 = 45; }
	\header{ piece = "Allemande"; }
}

