
\version "1.3.120";

\include "corno-1.ly"
\include "corno-2.ly"

corniStaff = \context Staff = corni <
	\property Staff.midiInstrument = #"french horn"

	\property Staff.instrument = #`((kern . 0.5)
	(lines "2 Corni" (rows "(E" ,text-flat ")")))
	
	\property Staff.instr = #`((kern . 0.5)
	(lines "Cor."  (rows "(E" ,text-flat ")")))

	% urg: can't; only My_midi_lexer:<non-static> () parses pitch?
	%\property Staff.transposing = "es"
	\property Staff.transposing = #3
	\Time
	\notes { \key c \major; }
	\End
	\context Voice=one \partcombine Voice
		\context Thread=one \cornoI
		\context Thread=two \cornoII
>

