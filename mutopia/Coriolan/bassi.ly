\header{
filename = 	 "bassi.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description = 	 "";
composer = 	 "Ludwig van Beethoven (1770-1827)";
enteredby = 	 "JCN";
copyright = 	 "public domain";
}

\version "1.3.117";

\include "global.ly"
\include "violoncello.ly"
\include "contrabasso.ly"

bassiGroup =  \context PianoStaff = bassi_group \notes <
        %\global
	\context StaffCombineStaff=oneBassi {
		\property StaffCombineStaff.midiInstrument = #"cello"
		\property StaffCombineStaff.instrument = #"Violoncello\ne\nContrabasso"
		\property StaffCombineStaff.instr = #"Vc."
		\clef "bass"; 
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}
	\context StaffCombineStaff=twoBassi {
		\property StaffCombineStaff.midiInstrument = #"contrabass"
		\property StaffCombineStaff.instrument = #"Contrabasso"
		\property StaffCombineStaff.instr = #"Cb."
%		\property StaffCombineStaff.transposing = #-12
		\clef "bass"; 
		\key es \major;
		\skip 1*314; 
		\bar "|."; 
	}

	\context StaffCombineStaff=oneBassi \partcombine StaffCombineStaff
		\context StaffCombineVoice=one \violoncello
		\context StaffCombineVoice=two \contrabasso
>
