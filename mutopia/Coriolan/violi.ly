\header{
filename =	 "violi.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.3.93";

\include "global.ly"
\include "viola-1.ly"
\include "viola-2.ly"

%{
violiGroup = \context PianoStaff = violi_group \notes <
	\context StaffCombineStaff=oneVioli {
		\property StaffCombineStaff.midiInstrument = #"viola"
		\property StaffCombineStaff.instrument = #"Viola"
		\property StaffCombineStaff.instr = #"Vla."
		\clef "alto"; 
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}
	\context StaffCombineStaff=twoVioli {
		\property StaffCombineStaff.midiInstrument = #"viola"
		\property StaffCombineStaff.instrument = #"Viola II"
		\property StaffCombineStaff.instr = #"Vla. II"
		\clef "alto"; 
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}

	\context StaffCombineStaff=oneVioli \partcombine StaffCombineStaff
		\context StaffCombineVoice=one \violaI
		\context StaffCombineVoice=two \violaII
>
%}

violiGroup = \notes \context VoiceCombineStaff = violi <
	\context VoiceCombineStaff=violi {
		\property VoiceCombineStaff.midiInstrument = #"viola"
		\property VoiceStaffCombineStaff.instrument = #"Viola"
		\property VoiceStaffCombineStaff.instr = #"Vla."
		\clef "alto"; 
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \violaI
		\context VoiceCombineThread=two \violaII
>

