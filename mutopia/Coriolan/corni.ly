
\version "1.3.120";

\include "corno-1.ly"
\include "corno-2.ly"

corniStaff = \context Staff = corni <
	\property Staff.midiInstrument = #"french horn"

	\property Staff.instrument = #`(lines
	  "2 Corni" (rows "(E" ,text-flat ")"))
	
	\property Staff.instr = #`(lines "Cor."  (rows "(E" ,text-flat ")"))
	\property Staff.transposing = #3
	\Time
	\notes { \key c \major; }
	\End
	\context Voice=one \partcombine Voice
		\context Thread=one \cornoI
		\context Thread=two \cornoII
>

