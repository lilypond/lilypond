
\version "1.3.120";

\include "viola-1.ly"
\include "viola-2.ly"

%{
violeGroup =  \context PianoStaff = viole_group \notes <
	\context StaffCombineStaff=oneViole {
		\property StaffCombineStaff.midiInstrument = #"viola"
		\property StaffCombineStaff.instrument = #"Viola"
		\property StaffCombineStaff.instr = #"Vla."
		
		%\clef "alto";
		% Ugh, clef broken in 1.3.125
		\property StaffCombineStaff.clefGlyph = #"clefs-C"
		\property StaffCombineStaff.clefPosition = #0

		\global
	}
	\context StaffCombineStaff=twoViole {
		\property StaffCombineStaff.midiInstrument = #"viola"
		\property StaffCombineStaff.instrument = #"Viola II"
		\property StaffCombineStaff.instr = #"Vla. II"
		
		%\clef "alto"; 
		% Ugh, clef broken in 1.3.125
		\property StaffCombineStaff.clefGlyph = #"clefs-C"
		\property StaffCombineStaff.clefPosition = #0

		\global
	}

	\context StaffCombineStaff=oneViole \partcombine StaffCombineStaff
		\context StaffCombineVoice=one \violaI
		\context StaffCombineVoice=two \violaII
>
%}

violeGroup =  \notes \context VoiceCombineStaff = viole <
	\context VoiceCombineStaff=viole {
		\property VoiceCombineStaff.midiInstrument = #"viola"
		\property VoiceCombineStaff.instrument = #"Viola"
		\property VoiceCombineStaff.instr = #"Vla."

		%\clef "alto"; 
		% Ugh, clef broken in 1.3.125
		\property VoiceCombineStaff.clefGlyph = #"clefs-C"
		\property VoiceCombineStaff.clefPosition = #0

		\global
	}
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \violaI
		\context VoiceCombineThread=two \violaII
>

