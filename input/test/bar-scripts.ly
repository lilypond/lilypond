
\version "1.3.4";

onestaff = \context Staff = foo\notes  {
	\property Staff.instr = instr
	\property Staff.instrument = instrument \mark "B";
	 c1 \mark "A"; \break c2  c2 \break
}

grstaff = \notes \context GrandStaff <
	\context Staff = bar {

	\property Staff.instr = instr
	
	 \mark "B"; \break c1 \mark "A"; c2  }
	\context Staff = bufl { c1 c2  }
>

scpaper = \paper {\translator {\OrchestralScoreContext}}
stpaper = \paper{ \translator {\BarNumberingStaffContext }}

scscore = \score { \grstaff \paper {
\scpaper
}}


stscore = \score { \onestaff \paper {
 \stpaper
}}

%\score {\stscore}
\score {\scscore
\header { title = "bar scripts"; }
}
