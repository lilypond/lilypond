
\version "1.3.120";

\include "clarinetto-1.ly"
\include "clarinetto-2.ly"

clarinettiStaff = \context Staff = clarinetti <
	\property Staff.midiInstrument = #"clarinet"

	\property Staff.instrument = #`(lines
	  "2 Clarinetti" (rows "(B" ,text-flat ")"))
	
	\property Staff.instr = #`(lines "Cl."  (rows "(B" ,text-flat ")"))
	\property Staff.transposing = #-2
	
	\Time
	\notes { \key f \major; }
	\End
	\context Voice=one \partcombine Voice
		\context Thread=one \clarinettoI
		\context Thread=two  \clarinettoII
>

