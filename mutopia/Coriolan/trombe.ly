
\version "1.3.120";

\include "trombo-1.ly"
\include "trombo-2.ly"

trombeStaff =  \context Staff = trombe <
	\context Staff=trombe {
		\property Staff.midiInstrument = #"trumpet"

		\property Staff.instrument = #`((kern . 0.5)
		(lines "2 Trombe" (rows "(C)")))
		\property Staff.instr = #`((kern . 0.5)
		(lines "Tbe." (rows "(C)")))

		\notes { \key c \major; }
		\End
	}
	\context Voice=one \partcombine Voice
		\context Thread=one \tromboI
		\context Thread=two \tromboII
>

