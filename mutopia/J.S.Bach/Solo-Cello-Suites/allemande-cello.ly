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

\version "1.0.14";

\include "allemande-urtext.ly";

allemande_cello_global = \notes{
	\time 4/4;
	\key f;
	\clef bass;
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

allemande_cello_scripts = \notes{
}

allemande_cello_staff = \type Staff <
	\$allemande
	\$allemande_cello_global
	\$allemande_cello_scripts
>

\score{
%{
	urg, this breaks auto-beam-engraver?
	really hairy bug: 1.1.23
	extra bars get encountered!!
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 4.0;
	}
%}
   	\$allemande_cello_staff
	% \paper{ \include "scs-paper.ly"; }
	\paper{

	        linewidth = 180.\mm;
		\translator { \BarNumberingStaffContext }
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
