
\version "1.3.120";

\include "oboe-1.ly"
\include "oboe-2.ly"

oboiStaff =  \context VoiceCombineStaff = oboi <
	\property VoiceCombineStaff.midiInstrument = #"oboe"
	\property VoiceCombineStaff.instrument = #"2 Oboi"
	\property VoiceCombineStaff.instr = #"Ob."
	\global
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \oboeI
		\context VoiceCombineThread=two \oboeII
>
