\header{
filename = 	 "bassi.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description = 	 "";
composer = 	 "Ludwig van Beethoven (1770-1827)";
enteredby = 	 "JCN";
copyright = 	 "public domain";
}

\version "1.3.120";

\include "global.ly"
\include "violoncello.ly"
\include "contrabasso.ly"

bassiGroup =  \context PianoStaff = bassi_group \notes <
        %\global
	\context StaffCombineStaff=oneBassi {
		\property StaffCombineStaff.midiInstrument = #"cello"
		%\property StaffCombineStaff.instrument = #"Violoncello\ne\nContrabasso"
		\property StaffCombineStaff.instrument = #'(lines "Violoncello" "e" "Contrabasso")
		\property StaffCombineStaff.instr = #"Vc."
		%\clef "bass";
		% Ugh, clef broken in 1.3.125
		\property StaffCombineStaff.clefGlyph = #"clefs-F"
		\property StaffCombineStaff.clefPosition = #2

		\key es \major;
		\skip 1*314; 
		\bar "|."; 
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
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}

	\context StaffCombineStaff=oneBassi \partcombine StaffCombineStaff
		\context StaffCombineVoice=oneBassi \violoncello
		\context StaffCombineVoice=twoBassi \contrabasso
>
