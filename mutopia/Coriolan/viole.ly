
\version "1.3.120";

\include "viola-1.ly"
\include "viola-2.ly"

%{
violeGroup =  \context PianoStaff = viole_group \notes <
	\context Staff=oneViole {
		\property Staff.midiInstrument = #"viola"
		\property Staff.instrument = #"Viola"
		\property Staff.instr = #"Vla."
		
		\clef "alto";
		%\property Staff.clefGlyph = #"clefs-C"
		%\property Staff.clefPosition = #0

		\global
	}
	\context Staff=twoViole {
		\property Staff.midiInstrument = #"viola"
		\property Staff.instrument = #"Viola II"
		\property Staff.instr = #"Vla. II"
		
		\clef "alto"; 
		%\property Staff.clefGlyph = #"clefs-C"
		%\property Staff.clefPosition = #0

		\global
	}

	\context Staff=oneViole \partcombine Staff
		\context Voice=one \violaI
		\context Voice=two \violaII
>
%}

violeGroup =  \notes \context Staff = viole <
	\context Staff=viole {
		\property Staff.midiInstrument = #"viola"
		\property Staff.instrument = #"Viola"
		\property Staff.instr = #"Vla."

		\clef "alto"; 
		%\property Staff.clefGlyph = #"clefs-C"
		%\property Staff.clefPosition = #0

		\global
	}
	\context Voice=one \partcombine Voice
		\context Thread=one \violaI
		\context Thread=two \violaII
>

