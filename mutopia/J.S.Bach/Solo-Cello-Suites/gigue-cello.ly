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

\version "1.1.66";

\include "gigue-urtext.ly";

gigue_cello_global = \notes{
	\time 3/8;
	\key f;
	\clef bass;
	\repeat "semi" 2 {
		\partial 8;
		s8
		s4.*31
		s4
		%\partial 4;
	} \repeat "semi" 2 {
		% urg
		%s16 \partial 8; s16
		s8
		s4.*43
		s4
		%\partial 4;
	}
}

gigue_cello_scripts = \notes{
}

gigue_cello_staff = \context Staff <
	\$gigue
	\$gigue_cello_global
	\$gigue_cello_scripts
>

\score{
	\$gigue_cello_staff
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 7.0;
		\translator{
			\VoiceContext
			beamAutoEnd_8 = "3/4";
			beamAutoEnd_16 = "3/4";
		}
	}
	\midi{ \tempo 4 = 60; }
	\header{ piece = "Gigue"; }
}

