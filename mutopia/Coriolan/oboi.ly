
\version "1.3.120";

\include "oboe-1.ly"
\include "oboe-2.ly"

oboiStaff =  \context Staff = oboi <
	\property Staff.midiInstrument = #"oboe"
	\property Staff.instrument = #"2 Oboi"
	\property Staff.instr = #"Ob."
	\global
	\context Voice=one \partcombine Voice
		\context Thread=one \oboeI
		\context Thread=two \oboeII
>
