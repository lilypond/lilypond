\header{
filename =	 "violi-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.59";

\include "global.ly"
\include "viola-1.ly"
\include "viola-2.ly"

violiGroup = \context PianoStaff = violi_group \notes <
	\context StaffCombineStaff=one {
		\clef "alto"; 
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}
	\context StaffCombineStaff=two {
		\clef "alto"; 
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}

	\context StaffCombineStaff=one \partcombine StaffCombineStaff
		\context StaffCombineVoice=one \violaI
		\context StaffCombineVoice=two \violaII
>

\score{
	\violiGroup
	\include "coriolan-part-combine-paper.ly"
	\include "coriolan-midi.ly"
}

