
\score  {
\notes {
	\context AutoSwitchGrandStaff \relative c' {
	c8 d b a' a, f' g,4 ~ g

	}
}

\paper {
\translator { \ScoreContext
\accepts AutoSwitchGrandStaff;
}
\translator{
	\type "Engraver_group_engraver";
	\name AutoSwitchGrandStaff;
	\consists "Span_bar_engraver";
	\consists "Vertical_align_engraver";
	\consists "Piano_bar_engraver";
	\consistsend "Axis_group_engraver";
	minVerticalAlign = 2.*\staffheight;
	maxVerticalAlign = 2.*\staffheight;	
	switcherName = "Voice";
	acceptorName = "Thread";
	staffContextName = "Staff";

	\accepts "AutoSwitchContext";
	\accepts "Staff";
}
\translator {
	\type "Engraver_group_engraver";
	\name "AutoSwitchContext";
	\consists "Staff_switching_translator";
}

}}

\version "1.2.0"; 
