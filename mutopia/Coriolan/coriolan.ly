\header{
filename =	 "coriolan.ly";
title =	 "Ouverture"; 
subtitle = "Coriolan";
opus = "Op. 62";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.59";

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


legniGroup = \context StaffGroup = legni_group <
	\flautiStaff
	\oboiStaff
	\clarinettiStaff
	\fagottiStaff
>

ottoniGroup = \context StaffGroup = otonni_group <
	\corniStaff
	\trombeStaff
>

timpaniGroup = \context StaffGroup = timpani_group <
	\timpaniStaff
	% Force a staff bracket (?)
	\context Staff = timpany { \skip 1*314; }
>

violiniGroup = \context GrandStaff = violini_group <
	\violinoIStaff
	\violinoIIStaff
>

violiGroup = \context PianoStaff = violi_group <
	\context StaffCombineStaff=one {
		\clef "alto"; 
		\skip 1*314; 
		\bar "|."; 
	}
	\context StaffCombineStaff=two {
		\clef "alto"; 
		\skip 1*314; 
		\bar "|."; 
	}

	\context StaffCombineStaff=one \partcombine StaffCombineStaff
		\context StaffCombineThread=one \violaI
		\context StaffCombineThread=two \violaII
>

bassiGroup = \context PianoStaff = bassi_group <
	\context StaffCombineStaff=one {
		\clef "bass"; 
		\skip 1*314; 
		\bar "|."; 
	}
	\context StaffCombineStaff=two {
		\clef "bass"; 
		\skip 1*314; 
		\bar "|."; 
	}

	\context StaffCombineStaff=one \partcombine StaffCombineStaff
		\context StaffCombineThread=one \violoncello
		\context StaffCombineThread=two \contrabasso
>


archiGroup = \context StaffGroup = archi_group <
	\violiniGroup
	\violiGroup
	\bassiGroup
>


\score{
	<
		\legniGroup
		\ottoniGroup
		\timpaniGroup
		\archiGroup
	>
	\header{
		title = "Coriolan";
		subtitle = "Ouverture"; 
		opus = "Opus 62";
		composer = "Ludwig van Beethoven (1770-1827)";
		enteredby = "JCN";
		copyright = "public domain";
	}
	\include "coriolan-paper.ly"
	\include "coriolan-midi.ly"
}

