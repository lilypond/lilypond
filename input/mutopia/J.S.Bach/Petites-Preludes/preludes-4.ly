#(ly:set-point-and-click! 'line-column)

opus = "BWV 925"
piece = "4"

\version "1.7.3"

%{

This stretches LilyPond capabilities a little.

%}

one = \context Staff \notes\relative c {
	\context Voice=i
	\property Voice.TextScript \set #'font-style = #'italic
%	r16_"legato" 
	r16
	\property Voice.TextScript \set #'font-style = #'finger
	d' fis d  a' b cis a 
	d cis d e  d fis e d |
	\property Voice.TextScript \set #'font-style = #'finger
	\stemUp\tieUp 
	cis4 fis ~ fis8 fis e4 ~ | 
	e16 \stemBoth\tieBoth a,16 cis a  d e fis d 
	g fis g a  g b a g |
	\stemUp

	fis4 e8 a <fis4 d> <gis e> |
	%5
	<a4 e> <fis d> < g!16 d> fis g a 
	\stemBoth
	g b a g |
	\property Voice.TextScript \set #'font-style = #'italic

	fis e fis g  fis a g fis 
	\property Voice.TextScript \set #'font-style = #'finger
%	e4-"2\\_5" ~ e16 e, fis g  |
	e4 ~ e16 e, fis g  |
	\property Voice.TextScript \set #'font-style = #'italic

	a g a b  a c b a 
	g fis g a  g b a g |
	\stemUp\tieUp
	fis4 
	\skip 4*7 |
	%10
	\property Voice.TextScript \set #'font-style = #'finger
%	d'4. cis8-"4\\_5" ~ cis b4 a8-"4\\_5" ~ |
	d'4. cis8 ~ cis b4 a8 ~ |
	\property Voice.TextScript \set #'font-style = #'italic

	a g4 fis8 ~ fis16 fis e d 
	\property Voice.TextScript \set #'font-style = #'finger
%	e4-"3\\_5" ~ |
	e4 ~ |
	e16 e d cis d4 ~ d16 d cis b cis4 |
	\stemBoth
	fis,16 a d c  b d g fis 
	d b' a g  fis e d c! |
	b d g a  fis8-\prall e16 d 
	d8. e16~  e d8 cis16 |
	%15
	\stemUp\tieUp
	r16 d fis d  g a b g  c b c d  c e d c |
	b4 a ~ a8 g ~  g16 fis8 e16 |
	fis8 e~  e d~ d4 cis |

	d2 ~ d16 a b cis d4 |
	\bar "|."
}

two = \context Staff \notes\relative c{
	\context Voice=ii
	\stemUp
	\property Voice.TextScript \set #'font-style = #'finger
	fis4 e8 a4 a4 gis8 | 
	 a8
	\translator Staff=upper \stemDown\tieDown

	a'4 a8 b4. cis8 ~ | 
	cis8
	\translator Staff=lower \stemUp\tieUp
	a,8 ~ a d ~ d d4-> cis8 | 
	d8
	\translator Staff=upper \stemDown\tieDown
	d'4-> cis8 ~ cis b4 b8 |
	%5
	r8 a4 a8
	\translator Staff=lower \stemUp\tieUp
	g8 fis e4 ~ | 
	e4 d ~ d16 d cis b cis4 ~ |
	cis8 a d4. d4^> cis8 |
	\translator Staff=upper \stemDown\tieDown
	d4 \stemBoth\tieBoth r16 b d b~ <g'4 e b> 
	r16 cis, e cis~ |
	<a'4 fis cis> r16 d, fis d~ <b'4 g d>
	r16 fis a fis~ |
	%10
	\stemDown
	fis4 e d cis |
	b a b4. b8 |
	a4
	\translator Staff=lower \stemUp
	r16 b fis a g4 r16 a e g |
	\skip 1*1 
	s16
	\translator Staff=upper \stemDown\tieDown
	d'8. ~ d8 c d4 a8 g |
	%15
	fis8 \translator Staff=lower \stemUp c' b4 \stemDown <a
	\translator Staff=upper \stemDown
	a'4.>
	\stemDown\tieDown
	a'8~ |
	a g~  g16 e fis8~  fis16 d8.~  d8. cis!16 |
%	\translator Staff=lower \stemUp
	\context Staff <
%		\context Voice=i { \stemUp\tieU  d8 a~ a4 }
%		\context Voice=ii { \stemDown a8 g  fis16 e fis d }
		\context Voice=ii {
		        \translator Staff = upper
		        \stemDown\tieDown d8 a~ a4
		}
		\context Voice=iii {
		        \translator Staff = lower
		        \stemUp a8 g fis16 e fis d
		}
	>
	\translator Staff=lower
	\stemUp\tieUp
	g fis g a  g b a g ~ |
	g g fis e fis4 ~ fis2
	\bar "|."
}

three = \notes\relative c{
   	\context Voice=iii
	\stemDown 
	d4 cis b e |
	a16 a, cis a  d e fis d 
	g fis g a  g b a g |
	fis4 fis e a |
	d16 \stemBoth d, fis d  a' b cis a 
	d cis d e  d fis e  d |
	%5
	cis b cis d  c e d c 
	\stemDown b4 cis8 b |
	a fis b a  gis e a g |
	fis4. d8 e4 a4 |
	\stemBoth
	r16 d, fis d  g8 g, 
	r16 e' g e  a8 a, |
	r16 fis' a fis  b8 b, r16 g' b g  d'8 d, |
	%10
	r16 d,16 fis d  a' b cis a  d b d b 
	fis' g a fis |
	fis16 g, b g  d' e fis d  g8 g, r16 cis e cis |
	fis8 fis, \stemDown r16 b d b e8 e,
	r16 a cis a |
	\stemBoth
	d e fis d  g fis e d  cis! a b cis 
	d e fis d |
	g8 e \stemDown\tieDown a a,  b8. g16 a4 |
	%15
	d8 a'~  a g~  g g fis4 |
	\stemUp
	r16 g b g  d'8. c16  b8. bes16  a8 g |
	\stemDown
	r16 a, cis! a'  d, e fis d  e8 d  e a, |
	d a~  a16 a b cis d2 |
}

four = \context Staff \notes\relative c{
	\skip 1*9
	\context Voice=iv
	\stemUp
	\property Voice.NoteColumn \override #'horizontal-shift = #1
	%10
	a''2 fis |
	d s |
	\skip 1*2 |
	s4
	\translator Staff=lower \stemUp\tieUp
	a4 ~ a16 d, g8  fis e |
	%15
	\stemDown\tieDown
	d1 ~ | 
	% we'll get some warnings, but it looks better
	\property Voice.NoteColumn \override #'horizontal-shift = #0
	d | 
	d, ~ |
	d2 d2
}

global = \notes{
	\time 4/4
	\key d \major
}

\score{
	% Allegretto
	\context PianoStaff <
		\context Staff = upper <
			\global
			\one
			\four
		>
		\context Staff = lower <
			\global
			\clef "bass"
			\two
			\three
		>
	>
	\paper{
		linewidth = 18.0 \cm
		\translator{
		        \VoiceContext
			% consider ending beam at every 1 2 note
			autoBeamSettings \override #'(end 1 8 * *) = #(ly:make-moment 1 4)
		}
	}
	\midi{ \tempo 4 = 70 }
	\header{
		opus = \opus
		piece = \piece
	}
}
