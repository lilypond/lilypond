\header{
filename =	 "coriolan.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.9";

\include "global.ly"

\include "contrabasso.ly"
\include "clarinetti.ly"
\include "corni.ly"
\include "fagotti.ly"
\include "flauti.ly"
\include "oboi.ly"
\include "timpani.ly"
\include "trombe.ly"
\include "viola.ly"
\include "violino-1.ly"
\include "violino-2.ly"
\include "violoncello.ly"

legno = \type Staff_group = legno_group <
	\$flauti_staff
	\$oboi_staff
	\$clarinetti_staff
	\$fagotti_staff
>

koper = \type Staff_group = koper_group <
	\$corni_staff
	\$trombe_staff
>

$timpani_g = \type Staff_group = timpani_group <
	\$timpani_staff
>

$violini = \type Grandstaff = violini_group <
	\$violino1_staff
	\$violino2_staff
>

$bassi = \type Grandstaff = violine_group <
	\$violoncello_staff
	\$contrabasso_staff
>

strijkers = \type Staff_group = strijkers_group <
	\$violini
	\$viola_staff
	\$bassi
>

\score{
	<
		\$legno
		\$koper
		\$timpani_g
		\$strijkers
	>
	\paper{}
	\midi{
		\tempo 4 = 160;
	}
}

