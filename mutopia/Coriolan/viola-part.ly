\header{
filename =	 "viola-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.1.52";

\include "global.ly"
\include "viola-1.ly"
\include "viola-2.ly"

$viola_staff = \context GrandStaff = viola <
	\$viola1_staff
	\$viola2_staff
>

\score{
	\$viola_staff
	%%\include "coriolan-part-paper.ly"
	\paper{
		castingalgorithm = \Wordwrap;
		\translator { \VoiceContext beamAuto=0; }
		\translator { \HaraKiriStaffContext }
		% \translator { \ScoreContext skipBars = 1; }
		\translator { \OrchestralScoreContext skipBars = 1; }
	}
	\midi{ \tempo 4 = 160; }
}

