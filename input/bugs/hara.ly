

The following example shows 2 bugs with HaraKiriStaffContext.

The first bug is that when the second staff disappears, the instr
string in the margin "2" is still printed, and surprisingly appears
above staff 1.

The second and more serious bug is that when staff 2 reappears,
the bar separators are no longer being printed for that staff.

Regards,
Roy Rankin

\header {
	composer = "Music: Trad.";
	crossRefNumber = "4";
	title = "HaraKiri Bugs";
}
voice1 = \notes {
\property Staff.instrument = "Part1"
\property Staff.instr = "1"
\property Staff.timeSignatureStyle="C"
 \time 4/4; \key c;   
 e''4.    e''8    e''4    r4   |   e''4    d''4   
 c''2      |
 e''4    d''4    c''2 (     |
 e''2    c''2   ~      |
 ) c''1    \bar "|."; \break       
 e''4.    e''8    e''4    r4   |   e''4    d''4   
 c''2      |
 e''4    d''4    c''2 (     |
 e''2    c''2   ~      |
 ) c''1    \bar "|.";       \break
 e''4.    e''8    e''4    r4   |   e''4    d''4   
 c''2      |
 e''4    d''4    c''2 (     |
 e''2    c''2   ~      |
 ) c''1    \bar "|."; \break       
 e''4.    e''8    e''4    r4   |   e''4    d''4   
 c''2      |
 e''4    d''4    c''2 (     |
 e''2    c''2   ~      |
 ) c''1    \bar "|.";       
}
voice2 = \notes {
\property Staff.instrument = "Part2"
\property Staff.instr = "2"
\property Staff.timeSignatureStyle="C"
 \time 4/4; \key c;   
 e''4.    e''8    e''4    r4   |   e''4    d''4   
 c''2     |
 e''4    d''4    c''2 (     |
 e''2    c''2   ~      |
 ) c''1    \bar "|.";       
s1 * 10
 e''4.    e''8    e''4    r4   |   e''4    d''4   
 c''2      |
 e''4    d''4    c''2 (     |
 e''2    c''2   ~      |
 ) c''1    \bar "|.";       
}
\score{
        \notes <

	\context Staff="1"
	{
	    \$voice1
	}
	\context Staff="2"
	{
	    \$voice2
	}

    >
	\paper 
	{
		\translator 
		{
			\HaraKiriStaffContext
			\consists  Staff_margin_engraver;
		}
	}
	\midi {}
}



