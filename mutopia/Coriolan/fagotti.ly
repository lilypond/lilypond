
\version "1.3.120";

\include "fagotto-1.ly"
\include "fagotto-2.ly"

fagottiStaff =  \context Staff = fagotti <
	\property Staff.midiInstrument = #"bassoon"
	\property Staff.instrument = #"2 Fagotti"
	\property Staff.instr = #"Fg."
	\clef "bass";
	%\property Staff.clefGlyph = #"clefs-F"
	%\property Staff.clefPosition = #2
	\global
	\context Voice=one \partcombine Voice
		\context Thread=one \fagottoI
		\context Thread=two \fagottoII
>

