#(ly:set-point-and-click 'line-column)

opus= "BWV 939"
piece = "2"


\version "1.7.18"
upper =  \context Staff \notes\relative c{
	\context Voice=i
	\property Voice.TextScript \set #'font-style = #'italic
	r8 c' e g  e c bes' g |
	a c, f a  f c c' a |
	b g b d  b g f' d |
	<<e c g>>2
	r4
	<<e c g >>
	<<d c a>>2
	r4
	<<d c a>>
	< { \stemUp [d8 g, 8 b d] }
	  \context Voice = ii < \stemDown b g >
	>
	
	<<b g>>4
	<<e b>>
	c2
	r4
	<
		{ \stemUp\tieUp c ~ c8}
		% We get a warning: No ties created!
		% but they are too!
		\context Voice=ii { \stemDown\tieDown <<e, a>>4 ~ <<e a>>8 }
	>
	\stemBoth 
	d, fis a  fis d c' a |
	b g b d  b g f'! d |
	e g, c e  c g g' e |
	fis a, c fis  c a a' fis |
	<<g d b>>2
	r4 b,4 |
	c8 e g c  g e bes' g |
	a16 g f e  d c b ! a 
	b g a b  c d e f |
	g4
	<<g, c>>2
	<<d b'>>4
	<<e c'>>1
	\bar "|."
}

lower =  \context Staff \notes\relative c{
	\context Voice=i
	<
		{ \stemUp\tieUp c1 ~ | c ~ | c }
		\context Voice=ii { \stemDown\tieDown c,1 ~ | c ~ | c }
	>
	\stemBoth |
	r8 c e g  e c c' e, |
	fis d fis a  fis d d' fis, |
	g2 r4 g |
	a8 a, c e  c a g' e |
	fis2 r4 d |
	g-\mordent-#'(music "accidentals-1") g, 4 g'-\mordent g, |
	% mordents in brackets...
	g'-\mordent g, g'-\mordent g, |
	g'-\mordent g, g'-\mordent g, |
	g'8 g, b d  b g f'! d |
	e c e g  e c c' e, |

	f1 ~ |
	f8 d e c g'4 g, |
	c,1
	\bar "|."
}

global =  \notes{
	\time 4/4
}

\score{
	\context GrandStaff <
		\context Staff = upper <
			\global
			\upper
		>
		\context Staff = lower <
			\global
			\clef "bass"
			\lower
		>
	>
	\paper{
	  linewidth = 18.0 \cm  
        }
	\midi{ \tempo 4 = 100 }
	\header {
		opus = \opus
		piece = \piece
	}
}

%% new-chords-done %%
