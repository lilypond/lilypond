\header{
texidoc="
Markings that are attached to (invisible) barlines are 
delicate: the are attached to the rest of the score without the score
knowing it.  Consequently, they fall over  often.
";
}



onestaff =  \context Staff = foo\notes  {
	\property Staff.instr = instr
	\property Staff.instrument = instrument \mark "B";
	 c1 \mark "A"; \break c2  c2 \break
}

grstaff =  \notes \context GrandStaff <
	\context Staff = bar {

	\property Staff.instr = instr
	
	 \mark "B"; \break c1 \mark "A"; c2  }
	\context Staff = bufl { c1 c2  }
>

scpaper =  \paper {\translator {\OrchestralScoreContext}}
% stpaper =  \paper{ \translator {\BarNumberingStaffContext }}
stpaper =  \paper{ }

scscore =  \score { \grstaff \paper {
\scpaper
}}


stscore =  \score { \onestaff \paper {
 \stpaper
}}

%\score {\stscore}
\score {\scscore
\header { title = "bar scripts"; }
}
