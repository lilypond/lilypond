\version "1.0.7";

toeter_i = \notes\relative c {
	\property Staff.instrument = "Toeters"
	\property Staff.instr = "Ttr."
	c'''4 c c c \break
	d d d d \break
	R1 \break
	\voiceone
	e4 e e e \break
	\onevoice
	f f f f \break
	g g g g
}

toeter_ii = \notes \relative c {
	c''4 c c c \break
	d d d d \break
	R1 \break
	\voicetwo
	e4 e e e \break
	\onevoice
	f f f f \break
	g g g g
}

toeters = \type StaffGroup = xtoeters <
	\type Staff = toeters <
	\toeter_i
	\toeter_ii
	>
>
 
zager = \type Staff = zager \notes \relative c {
	\clef bass;
	\property Staff.instrument = "Zager"
	\property Staff.instr = "Zag."
	c4 d e f \break
	\property Staff.instr = "Zag. \& Zog."
	f e d c \break
	c d e f \break
	\property Staff.instr = "Zag."
	\voiceone
	f e d c \break
	\onevoice
	c d e f \break
	f e d c
}

zoger = \type Staff = zoger \notes \relative c {
	\clef bass;
	\property Staff.instrument = "Zoger"
	\property Staff.instr = "Zog."
	c4 d e f \break
	\skip 1*2;
%	\staffone
	\translator Staff=zager
	\voicetwo
	c2 g2\break
	\onevoice
	a4 b c d \break
	\skip 1*1;
}

 \include "score-paper.ly";

zagers = \type GrandStaff <
	\zager
	\zoger
>
 
\score{
	<
		\type StaffGroup = toeters <
			\toeters
		>
		\type StaffGroup = zagers <
			\zagers
		>
	>
	\paper{
		linewidth = 40.0\mm;
	}
}


