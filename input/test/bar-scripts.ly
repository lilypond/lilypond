


\version "1.0.7";

onestaff = \type Staff = foo\notes  {
	\property Staff.instr = instr
	\property Staff.instrument = instrument \mark "B";
	 c1 \mark "A"; \break c2  c2 \break
}

grstaff = \notes \type GrandStaff <
	\type Staff = bar {

	\property Staff.instr = instr
	
	 \mark "B"; \break c1 \mark "A"; c2  }
	\type Staff = bufl { c1 c2  }
>

scpaper =  \paper {\translator {\OrchestralScoreContext}}


stpaper =\paper{ \BarNumberingStaffContext }
scscore = \score { \grstaff \paper {
\scpaper
}}


stscore = \score { \onestaff \paper {
 \stpaper
}}

\score {\stscore}
%\score {\scscore}
