\header{
filename =	 "bassi-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.0.7";

\include "global.ly"
\include "violoncello.ly"
\include "contrabasso.ly"

$bassi = \type GrandStaff = bassi_group <
	\$violoncello_staff
	\$contrabasso_staff
>


$bassi = \type GrandStaff = bassi_group <
	\$violoncello_staff
	\$contrabasso_staff
>

\score{
	\$bassi
	\include "coriolan-part-paper.ly"
	\midi{ \tempo 4 = 160; }
}

