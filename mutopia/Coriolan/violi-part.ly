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

violiGroup = \context GrandStaff = violi_group <
	\context Staff=one { 
		\clef "alto"; 
		\context Voice
		\property Voice.soloADue = ##f 
		\skip 1*314; 
		\bar "|."; 
	}
	\context Staff=two { 
		\clef "alto"; 
		\context Voice
		\property Voice.soloADue = ##f 
		\skip 1*314; 
		\bar "|."; 
	}
	\context Staff=one \partcombine Staff
		\context Thread=one \violaI
		\context Thread=two \violaII
>

%\include "coriolan-part-paper.ly"
\include "coriolan-part-combine-paper.ly"

\score{
	\violiGroup
	\paper{
		\translator { \HaraKiriStaffContext }
		\translator {
			\StaffContext
			\consists "Slur_engraver";
			\consists "Rest_engraver";
			\consists "Tie_engraver";
		}
		\translator{
			\VoiceContext
			soloADue = ##f 
			\remove "Rest_engraver";
			\remove "Slur_engraver";
			\remove "Tie_engraver";
		}
	}
	\include "coriolan-midi.ly"
}

