
\header {
composer = "Georg Philipp Telemann";
title = "Musique de Table";
date = "1733";
tempo = "Maestoso";
}

%{

This appeared in Uijlenspieghel, the magazine of the Dutch Horn
Society, spring issue.

It is part of 3rd production of the Table Music by Telemann. It is a
concerto for two "Tromba Selvatica" (Wood trumpets, ie. horns.) in
E-flat.

There probably are violin parts as well. (they weren't printed in
Uijlenspieghel)


%}




trombaUno = \notes \relative  c'' {
c2. ~
c4
g  c |
e2. ~ e4 c e | 
g2. ~ g4
f2 ~
f4
e2~  e4
f2 ~ f4
d4 e ~ |
e8 f d4.-\trill c8
c e e-\trill d d16 e d c  |
c4 r r |
r8 e f f16 g e8 e16 f | 
d4 r r
r8 d g g16 e f8 f16 d | 
e16 g g e g e d e f32 g f16 d32 e d16
e16 g g e g e d e f32 g f16 d32 e d16 |
e16 d c d e d e d e8 fis g4 r r |
R2.
R2.
r4 d2~ d4 r r R2.
d2. ~ d 4 g, d'
d2. ~ d4 g, d' d2. e fis g
a4. a8 g4 ~ g8 e fis4.-\trill g8
g g16 a b c d4 c8 b4
r r
g c b -\trill c r r |
r8 [c16 b] c8 c16 d  b8 b16 c |
a4 r r | r e2~ e4 r r |  r e2~ e4 d r | r c2 ~ c4 d r R2. |
r4 e8 e e e e4 r r
r8 [e e-\trill d] d16 e d c| c4 r r|
r8 e8 f f16 g e8 e16 f |
d8 d4 f8 e16 d c e d8 d4 f8 e16 d c e
d4 r r |
r8 d g g16 e f8 f16 d |
e16 g g e g e d e f32 g f16 d32 e d16 |
e16 g g e g e d e f32 g f16 d32 e d16 |
e16 d c d e f e d e8 d-\trill c4 r r r8 e f f16 g e8 e16 f
d8 d4 f8 e16 d c e d8 d4 f8 e16 d c e
d4 r r| 
c2. ~ c4 g c e2.~ e4 c e |
g2. ~ g4 f2 ~ f4 e2~e4 f2~f2.
r4 d e~ e8 f d4.-\trill c8
c2 r4
e16 g g e c e e c g c c g e4 r r c4-| e4-| g-| c,2.


}


trombaDue = \notes \relative c' {
e2. ~e4 c e g2.~g4 e g8 c |
e2. ~ e4 d2~d4 d2 c2. d4 g, g  |
r4 g8 g g g e c' c g g16 e g8 |
e4 r r
r8 c' d8 d16 e c8 c, | g'4 r r |
r4 r8 g d' d16 g, |
c16 e e c   e c g c   d d g, g |
c e e c     e c g c   d d g, g |
c g e g     c g c g   c8 d  |
d4 r r R2. R2. R2. |
r4 d2 ~ d4 r r |
g,2. ~ g4 d' g, | g2. ~ g4 d' g, | g2. ~ g4 c e | r d2 ~ d4 c4 e8. d16  |
d4. d8 d4 |
r8 e d4 d | d g fis-\trill | g r r |
r8 [c,16 d] e f g4 f8 e4 r r |
r8 [e16 d] e8 e16 f d8 d16 e
c4 r r | R2. | r4 e2~e4 r r  | r d2 ~ d4 c4 r4 |
r4 d2~ d4 r  r |
r [e8 e e e] e4 r r |
r8 [c8 c g] g16 e g8 e4 r r
r8 c'8 d8 d16 e c8 c, g'8 g4 d'8 c16 g e c g'8 g4 d'8 c16 g e c
g'4 r r |
r4 r8 g d' d16 g, |
c16 e e c   e c g c  d d g, g
c   e e c   e c g c  d d g, g |
[c16 g e g] [c8 c16  c] [c g g32 e32 g16] |
e4 r r | r8 c' d d16 e c8 c, g'8 g4 d'8 c16 g e c |
g'8 g4 d'8 c16 g e c | g'4 r r |
e2.~e4 c e | g2.~g4 e g8 c | e2.~e4 d2 ~ d4 d2 c2. d2. r4 g,4 g r g8 g g g |
e2 r4
c'16 e e c g c c g e g g e c4 r r c-| e-| g-| c2. 
}

global = \notes {
	\time 3/4;
	s2.* 80
	\bar "|.";
}
trbname = "Horn (E\\textflat)"
trbnameI = \trbname + " 1     "
trbnameII = \trbname + " 2     "

\score {
	\context GrandStaff <
	 \context Staff = one <
		\trombaUno
		\property Staff.instrument = \trbnameI
		\property Staff.instr = " "
		\global
	>
	\context Staff = two <
		\property Staff.instrument = \trbnameII
		\property Staff.instr = " "
		\global
		\trombaDue
	>
	>
	\paper {
		 \translator  {
		 	\OrchestralScoreContext
			minVerticalAlign = 2.5*\staffheight;
		}
		\translator {
			\StaffContext
			\consists Staff_margin_engraver;
		}

%		castingalgorithm = \Wordwrap;
		indent = 2. \cm;
	}
}
