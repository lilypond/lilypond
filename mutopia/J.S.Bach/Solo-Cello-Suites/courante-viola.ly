\header{
filename =	 "courante-viola.ly";
title =	 "Solo Cello Suites";
subtitle = "Suite II";
% piece = "Courante";
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

\include "courante-urtext.ly";

courante_viola_global = \notes{
	\time 3/4;
	\key f;
	\clef alto;
	\repeat 2 {
		\partial 16;
		s16
		s2.*15
		% hmm
		s2 s8
		\partial 16*11;
		s16
	} \repeat 2 {
		% urg
		s32 \partial 16; s32
		s2.*15
		s16*11
		\partial 16*11;
	}
}

courante_viola_scripts = \notes{
}

courante_viola_staff = \type Staff <
	\notes \transpose c' \$courante
	\$courante_viola_global
	\$courante_viola_scripts
>

\score{
	\$courante_viola_staff
	%\paper { \include "scs-paper.ly"; }
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
	\midi{ \tempo 4 = 55; }
	\header{ piece = "Courante"; }
}

