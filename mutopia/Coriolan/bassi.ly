\version "1.3.120";

\include "violoncello.ly"
\include "contrabasso.ly"

%{
bassiGroup =  \context PianoStaff = bassi_group \notes <
        \staffCombinePianoStaffProperties
	\context Staff=oneBassi {
		\property Staff.midiInstrument = #"cello"
		\property Staff.instrument = #'(lines
    		  "Violoncello" "    e" "Contrabasso")

    		\property Staff.instr = #"Vc."
		\clef "bass";
		%\property Staff.clefGlyph = #"clefs-F"
		%\property Staff.clefPosition = #2

		\global
	}
	\context Staff=twoBassi {
		\property Staff.midiInstrument = #"contrabass"
		\property Staff.instrument = #"Contrabasso"
		\property Staff.instr = #"Cb."
		\property Staff.transposing = #-12
	 	\clef "bass"; 
		%\property Staff.clefGlyph = #"clefs-F"
		%\property Staff.clefPosition = #2

		\global
	}
	\context Staff=oneBassi \partcombine Staff
		\context Voice=oneBassi \violoncello
		\context Voice=twoBassi \contrabasso
>
%}

bassiGroup =  \context PianoStaff = bassi_group \notes <
	\context Staff=violoncelloStaff <
		\property Staff.midiInstrument = #"cello"
		\property Staff.instrument = #"Violoncello  "
    		\property Staff.instr = #"Vc.  "
		\clef "bass";
		\global
		\violoncello
	>
	\context Staff=contrabassoStaff <
		\property Staff.midiInstrument = #"contrabass"
		\property Staff.instrument = #"Contrabasso  "
		\property Staff.instr = #"Cb.  "
		\property Staff.transposing = #-12
	 	\clef "bass"; 
		\global
		\contrabasso
	>
>
