\header{
filename =	 "viola-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.4";

\include "global.ly"
\include "viola-1.ly"
\include "viola-2.ly"

$viola_staff = \context GrandStaff = viola <
	\$viola1_staff
	\$viola2_staff
>

\include "coriolan-part-paper.ly"

\score{
	\$viola_staff
	\include "coriolan-part-paper.ly"
	\paper{
		\translator { \HaraKiriStaffContext }
	}
	\include "coriolan-midi.ly"
}

