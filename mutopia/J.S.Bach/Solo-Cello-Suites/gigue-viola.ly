\header{
filename =	 "gigue-viola.ly";
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

gigue_viola_global = \notes{
	\time 3/8;
	\key f;
	\clef alto;
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

gigue_viola_scripts = \notes{
}

gigue_viola_staff = \type Staff <
	\notes \transpose c' \$gigue
	\$gigue_viola_global
	\$gigue_viola_scripts
>

\score{
	\$gigue_viola_staff
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 7.0;
		\translator{
			\VoiceContext
			beamAutoEnd8 = "3/4";
			beamAutoEnd16 = "3/4";
		}
	}
	\midi{ \tempo 4 = 60; }
	\header{ piece = "Gigue"; }
}

