
\version "1.3.120";

\include "trombo-1.ly"
\include "trombo-2.ly"

trombeStaff =  \context VoiceCombineStaff = trombe <
	\context VoiceCombineStaff=trombe {
		\property VoiceCombineStaff.midiInstrument = #"trumpet"

		\property VoiceCombineStaff.instrument = #`((kern . 0.5)
		(lines "2 Trombe" (rows "(C)")))
		\property VoiceCombineStaff.instr = #`((kern . 0.5)
		(lines "Tbe." (rows "(C)")))

		\notes { \key c \major; }
		\End
	}
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \tromboI
		\context VoiceCombineThread=two \tromboII
>

