\header{
filename =	 "sarabande-viola.ly";
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

\version "1.0.14";

\include "sarabande-urtext.ly";

sarabande_viola_global = \notes{
	\time 3/4;
	\key f;
	\clef alto;
	\repeat 2 {
		s2.*12
	} \repeat 2 {
		s2.*16
	}
}

sarabande_viola_scripts = \notes{
}

sarabande_viola_staff = \type Staff <
	\notes \transpose c' \$sarabande
	\$sarabande_viola_global
	\$sarabande_viola_scripts
>

\score{
	\$sarabande_viola_staff
	% \paper { \include "scs-paper.ly"; }
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
	\midi{ \tempo 4 = 40; }
	\header{ piece = "Sarabande"; }
}

