\header{
filename =	 "coriolan.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.4";

\include "global.ly"
\include "paper16.ly"

\include "contrabasso.ly"
\include "clarinetti.ly"
\include "corni.ly"
\include "fagotti.ly"
\include "flauti.ly"
\include "oboi.ly"
\include "timpani.ly"
\include "trombe.ly"
\include "viola-1.ly"
\include "viola-2.ly"
\include "violino-1.ly"
\include "violino-2.ly"
\include "violoncello.ly"


legni = \context StaffGroup = legni_group <
	\$flauti_staff
	\$oboi_staff
	\$clarinetti_staff
	\$fagotti_staff
>

ottoni = \context StaffGroup = otonni_group <
	\$corni_staff
	\$trombe_staff
>

$timpani_g = \context StaffGroup = timpani_group <
	\$timpani_staff
	\context Staff = urgtimpany \notes{ \skip 1*34; }
>

$violini = \context GrandStaff = violini_group <
	\$violino1_staff
	\$violino2_staff
>

$violi = \context GrandStaff = violi_group <
	\$viola1_staff
	\$viola2_staff
>

$bassi = \context GrandStaff = bassi_group <
	\$violoncello_staff
	\$contrabasso_staff
>

archi = \context StaffGroup = archi_group <
	\$violini
	\$violi
	\$bassi
>


\score{
	<
		%%? \property Score . textStyle =  "italic"
		\$legni
		\$ottoni
		\$timpani_g
		\$archi
	>
	\include "coriolan-paper.ly"
	\include "coriolan-midi.ly"
}

