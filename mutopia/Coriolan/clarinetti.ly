
\version "1.3.120";

\include "clarinetto-1.ly"
\include "clarinetto-2.ly"

clarinettiStaff = \context VoiceCombineStaff = clarinetti <
	\property VoiceCombineStaff.midiInstrument = #"clarinet"

	\property VoiceCombineStaff.instrument = #`((kern . 0.5)
	(lines "2 Clarinetti" (rows "(B" ,text-flat ")")))
	
	\property VoiceCombineStaff.instr = #`((kern . 0.5)
	(lines "Cl."  (rows "(B" ,text-flat ")")))

	% urg: can't; only My_midi_lexer:<non-static> () parses pitch?
	%\property VoiceCombineStaff.transposing = "bes"
	\property VoiceCombineStaff.transposing = #-2
	
	\Time
	\notes { \key f \major; }
	\End
	\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
		\context VoiceCombineThread=one \clarinettoI
		\context VoiceCombineThread=two  \clarinettoII
>

