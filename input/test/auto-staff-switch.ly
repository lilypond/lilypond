
\score  {
\notes {
	\type AutoSwitchGrandStaff \relative c' {
	c8^8 d^8 b^8 a^8 [a,^8 f'^8 g,^8 ~ g]

	}
}

\paper {
\translator { \ScoreContext
\accepts AutoSwitchGrandStaff;
}
\translator{
	\type "Line_group_engraver_group";
	\name AutoSwitchGrandStaff;
	\consists "Span_bar_engraver";
	\consists "Vertical_align_engraver";
	\consists "Piano_bar_engraver";
	minVerticalAlign = 2.*\staffheight;
	maxVerticalAlign = 2.*\staffheight;	

	\accepts "AutoSwitchContext";
	\accepts "Staff";
}
\translator {
	\type "Engraver_group_engraver";
	\name "AutoSwitchContext";
	\consists "Staff_switching_translator";
}

}}
