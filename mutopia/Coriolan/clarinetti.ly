
\version "1.3.120";

\include "clarinetto-1.ly"
\include "clarinetto-2.ly"

clarinettiStaff = \context Staff = clarinetti <
	\property Staff.midiInstrument = #"clarinet"

	\property Staff.instrument = #`((kern . 0.5)
	(lines "2 Clarinetti" (rows "(B" ,text-flat ")")))
	
	\property Staff.instr = #`((kern . 0.5)
	(lines "Cl."  (rows "(B" ,text-flat ")")))

	% urg: can't; only My_midi_lexer:<non-static> () parses pitch?
	%\property Staff.transposing = "bes"
	\property Staff.transposing = #-2
	
	\Time
	\notes { \key f \major; }
	\End
	\context Voice=one \partcombine Voice
		\context Thread=one \clarinettoI
		\context Thread=two  \clarinettoII
>

