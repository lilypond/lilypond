\version "1.1.66";

toeter_i = \notes\relative c <{
		\property Staff.instrument = "Toeters"
		\property Staff.instr = "Ttr." }
	\context Voice = lower { \stemdown s1*6 }
	\context Voice = upper { \stemup s1*6 }
	\context Voice = together  { 

	c'''4 c c c \break
	d d d d \break
	R1 \break 
	\context Voice = upper {
		e4 e e e \break }
	f f f f \break
	g g g g 
}>

toeter_ii = \notes \relative c \context Voice = together { 
	c''4 c c c \break
	d d d d \break
	R1 \break
	\context Voice = lower {
		\stemdown 
		e4 e e e \break
        }
	f f f f \break
	g g g g
}

toeters = \context Staff = toeters <
	\toeter_i
	\toeter_ii
>
 
zager = \context Staff = zager \notes \relative c'' {
	\clef treble;
	\property Staff.instrument = "Zager"
	\property Staff.instr = "Zag."
	c4 d e f \break
	\property Staff.instr = "Zag. \& Zog."
	f e d c \break
	c d e f \break
	\property Staff.instr = "Zag."
	\stemup
	f e d c \break
	c d e f \break
	f e d c
}

zoger = \context Staff = zoger \notes \relative c'' {
	\clef treble;
	\property Staff.instrument = "Zoger"
	\property Staff.instr = "Zog."
	c4 d e f \break
	\skip 1*2;

	\translator Staff=zager
	\stemdown 
	c2 g2
	
	a4 b c d \break
	\skip 1*1;
}

zagers = \context GrandStaff <
	\zager
	\zoger
>
 
\score{
	<
		\context StaffGroup = toeters <
			\toeters
		>
		\context StaffGroup = zagers <
			\zagers
		>
	>
	\paper{
		linewidth = 40.0\mm;
		\translator { \HaraKiriStaffContext }
%uh?
		\translator { \OrchestralScoreContext }
%		\translator { \OrchestralPartStaffContext }
	}
}


