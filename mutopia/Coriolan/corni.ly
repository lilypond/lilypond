
\version "1.3.120";

\include "corno-1.ly"
\include "corno-2.ly"

corniStaff = \context VoiceCombineStaff = corni <
	\property VoiceCombineStaff.midiInstrument = #"french horn"

	\property VoiceCombineStaff.instrument = #`((kern . 0.5)
	(lines "2 Corni" (rows "(E" ,text-flat ")")))
	
	\property VoiceCombineStaff.instr = #`((kern . 0.5)
	(lines "Cor."  (rows "(E" ,text-flat ")")))

	% urg: can't; only My_midi_lexer:<non-static> () parses pitch?
	%\property VoiceCombineStaff.transposing = "es"
	\property VoiceCombineStaff.transposing = #3
	\Time
	\notes { \key c \major; }
	\End
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \cornoI
		\context VoiceCombineThread=two \cornoII
>

