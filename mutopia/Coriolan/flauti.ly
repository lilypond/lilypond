
\version "1.3.120";

\include "flauto-1.ly"
\include "flauto-2.ly"

flautiStaff =  \notes \context VoiceCombineStaff = flauti <
	\property VoiceCombineStaff.midiInstrument = #"flute"
	\property VoiceCombineStaff.instrument = #"2 Flauti"
	\property VoiceCombineStaff.instr = #"Fl."
	\global
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \flautoI
		\context VoiceCombineThread=two \flautoII
>

