#(set! point-and-click line-column-location)

opus = "BWV 940"
piece = "6"

\version "1.5.68"



one =  \context Staff \notes\relative c{
	\context Voice=i
	\skip 1 |
	\stemUp
	r4 d''2 cis4 |
	\stemBoth
	d16 a' g f  e f cis d 
	e8.-\mordent f16  d8.-\prall cis16 |
	\stemUp\tieUp
	cis4 ~ cis16 a d8 ~ d4 a |
	%5
	b2 ~ b4 a ~ |
	a16 a \stemUp g! f g4 ~ g f ~ |
	f16 a g f  e16 g8. ~  g16 g f e  d f8. ~ |
	f16 f e d b'4 a g |

	fis4 g r8 g16 bes e4 |
	%10
	d1
	\bar "|."
}

two =  \context Staff \notes\relative c{
	\context Voice=ii
	r16 d'' c bes  a bes f g 
	a8.-\mordent bes16  g8.-\prall f16 |
	\stemDown
	f2 e2 |
	\translator Staff=lower \stemUp
	r4 a, bes b |
	\translator Staff=upper \stemDown\tieDown
	r16 b' a g  f8. f16 e2 ~ |
	%5
	e2 ~ e4 ~ e16 e f! d |
	s4 e32 d e8.~ e4 d4 ~ |
	d4. cis16 d cis4 d ~ |
	d8 r r16 e f d r16 e f d r d e cis |
	r16 e d c!  bes! d8. s4 r16 bes' a g |
	%10
	fis1
}

three =  \context Staff \notes\relative c{
	\context Voice=iii
	\stemUp
	f2 e |
	\stemBoth
	d16 d' c bes  a bes f g 
	a8.-\mordent bes16 
	g8.-\prall f16 |
	f2 g4 gis |
	a2 ~ a16 a g f  e f c d |
	%5
	e8.-\mordent f16  d8.-\prall c16 \stemBoth c4. d8 |
	\stemDown
	e4 ~ e16 f e d  cis a b cis  d e f d |
	\property Voice.TextScript \set #'font-style = #'finger
	bes!2 a ~ |
	a a |
	d, cis' |
	%10
	a'1
	\bar "|."
}

four =  \context Staff \notes\relative c{
	\context Voice=iv
	\stemDown 
	d2 cis |
	\skip 1*2 |
	\skip 4*3
	\translator Staff=upper \stemUp
	\property Voice.NoteColumn \override #'horizontal-shift = #1
	c''4 |
	%5
	a gis ~ gis16 gis fis e 
	\skip 4*1
	\translator Staff=lower \stemDown
	\property Voice.NoteColumn \override #'horizontal-shift = #0
	\stemUp\tieUp
	b2 a |
	g a4. gis16 a |
	gis2 < g8 cis,> <f d> e4 |
	d4. fis16 g r16 bes8. ~ bes4 |
	%10
	\stemDown
	d,1
}

global =  \notes{
	\time 4/4
	\key f \major
}

\score{
	% Allegretto
	\context PianoStaff <
		\context Staff = upper <
			\global
			\one
			\two
		>
		\context Staff = lower <
			\global
			\clef "bass"
			\three
			\four
		>
	>
	\paper{
		linewidth = 18.0 \cm  

	}
	\midi{ \tempo 4 = 40 }
	\header{
		piece = \piece
		opus = \opus
	}
}

