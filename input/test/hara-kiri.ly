

toeterI =  \notes\relative c <{
		\property Staff.instrument = #"Toeters"
		\property Staff.instr = #"Ttr." }
	\context Voice = lower { \stemDown s1*6 }
	\context Voice = upper { \stemUp s1*6 }
	\context Voice = together  { 

	c'''4^"toet I" c c c 
	d d d d \break
	R1 
	\context Voice = upper {
		e4 e e e \break }
	f f f f
	g g g g 
}>

toeterIi =  \notes \relative c \context Voice = together { 
	c''4^"toet II" c c c 
	d d d d 
	R1 
	\context Voice = lower {
		\stemDown 
		e4 e e e 
        }
	f f f f 
	g g g g
}

toeters =  \context Staff = toeters <
	\toeter_i
	\toeter_ii
>
 
zager =  \context Staff = zager \notes \relative c'' {
	\clef treble;
	\property Staff.instrument = #"Zager"
	\property Staff.instr = #"Zag."
	c4^"zag" d e f 
	\property Staff.instr = #"Zag. \\& Zog."
	f e d c 
	c d e f 
	\property Staff.instr = #"Zag."
	\stemUp
	f e d c 
	c d e f 
	f e d c
}

zoger =  \context Staff = zoger \notes \relative c'' {
	\clef treble;
	\property Staff.instrument = #"Zoger"
	\property Staff.instr = #"Zog."
	c4^"zog" d e f 
	\skip 1*2;

	\translator Staff=zager
	\stemDown 
	c2 g2
	
	a4 b c d 
	\skip 1*1;
}

zagers =  \context GrandStaff <
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
		linewidth = 80.0\mm;
		\translator { \HaraKiriStaffContext }
%uh?
		\translator { \OrchestralScoreContext }
%		\translator { \OrchestralPartStaffContext }
	}
}


