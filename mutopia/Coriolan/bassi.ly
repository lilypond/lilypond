\version "1.3.120";

\include "violoncello.ly"
\include "contrabasso.ly"

bassiGroup =  \context PianoStaff = bassi_group \notes <
	\context StaffCombineStaff=oneBassi {
		\property StaffCombineStaff.midiInstrument = #"cello"
		\property StaffCombineStaff.instrument = #'((kern . 0.5)
    		(lines "Violoncello" (rows "    e") (rows "Contrabasso")))

    		\property StaffCombineStaff.instr = #"Vc."
		%\clef "bass";
		% Ugh, clef broken in 1.3.125
		\property StaffCombineStaff.clefGlyph = #"clefs-F"
		\property StaffCombineStaff.clefPosition = #2

		\global
	}
	\context StaffCombineStaff=twoBassi {
		\property StaffCombineStaff.midiInstrument = #"contrabass"
		\property StaffCombineStaff.instrument = #"Contrabasso"
		\property StaffCombineStaff.instr = #"Cb."
%		\property StaffCombineStaff.transposing = #-12
	 	%\clef "bass"; 
		% Ugh, clef broken in 1.3.125
		\property StaffCombineStaff.clefGlyph = #"clefs-F"
		\property StaffCombineStaff.clefPosition = #2

		\global
	}

	\context StaffCombineStaff=oneBassi \partcombine StaffCombineStaff
		\context StaffCombineVoice=oneBassi \violoncello
		\context StaffCombineVoice=twoBassi \contrabasso
>
