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

\version "1.0.14";

\include "gigue-urtext.ly";

gigue_cello_global = \notes{
	\time 3/8;
	\key f;
	\clef bass;
	\repeat 2 {
		\partial 8;
		s8
		s4.*31
		s4
		\partial 4;
	} \repeat 2 {
		% urg
		s16 \partial 8; s16
		s4.*43
		s4
		\partial 4;
	}
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
	% \paper { \include "scs-paper.ly"; }
	\paper{
	        linewidth = 180.\mm;
		\translator { \BarNumberingStaffContext }
		\translator{
			\VoiceContext
			% add experimental auto-beaming
			\consists Auto_beam_engraver;
			beamAuto = 1.;
			beamAutoEnd8 = "3/4";
			beamAutoEnd16 = "3/4";
		}
	}
	\midi{ \tempo 4 = 60; }
	\header{ piece = "Gigue"; }
}

