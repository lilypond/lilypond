\header{
filename =	 "bassi-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.59";

\include "global.ly"
\include "violoncello.ly"
\include "contrabasso.ly"

bassiGroup = \context PianoStaff = bassi_group \notes <
        %\global
	\context StaffCombineStaff=one {
		\clef "bass"; 
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}
	\context StaffCombineStaff=two {
		\clef "bass"; 
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}

	\context StaffCombineStaff=one \partcombine StaffCombineStaff
		\context StaffCombineVoice=one \violoncello
		\context StaffCombineVoice=two \contrabasso
>

\score{
	\bassiGroup
	\include "coriolan-part-combine-paper.ly"
	\include "coriolan-midi.ly"
}

