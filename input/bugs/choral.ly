
\include "deutsch.ly"

global = \notes {
	\time 2/2; \partial 2*1;
	\key g;

	s2 | s1 | s1 |
	\time 3/2;
	s1. | s1. |
	\time 2/2;
	s1 | s1 | s1 | s2

	s2 | s1 | s1 |
	\time 3/2;
	s1. | s1. |
	\time 2/2;
	s1 | s1
	\time 3/2;
	s1. \bar "||";

	s1. | s1. | s1. | s1. |
	s1. | s1. | s1. | s1. \bar "||";

	\key es;
	s1. | s1. | s1. | s1. |
	s1. | s1. | s1. | s1 \bar "||";

	\key g;
	s2 |
	\time 2/2;
	s1 | s1 |
	\time 3/2;
	s1. | s1. |
	\time 2/2;
	s1 | s1 | s1 | s2

	s2 | s1 | s1 |
	\time 3/2;
	s1. | s1. |
	\time 2/2;
	s1 | s1 | s1 \bar "|.";
}

themeMelody = \notes \relative c'' {
	g2 | g d' | h a |
	g fis g | a ~ a \breathe b |
	d b | a a | g ~ g ~ | g \breathe

	g2 | g d' | h a |
	g fis g | a ~ a \breathe b |
	d b |
}

themeBeforeBreak1EndMelody = \notes \relative  c'' {
	 a2 a | g ~ g ~ g4 r4
}

themeFinalEndMelody = \notes \relative  c'' {
	 a2 a ~ | a1^\fermata
}

themeRightHand = \notes \relative c' {
	d2 | c <e g> | <e g> <d fis> | d2 es es4 c4 |
	<d1 e1> <d2 f2> | <g2 b2> f2 | <cis2 e2> <e2 g2> |
	<d2 f2> <c4 f4> cis4 | <dis2 fis2>

	<c2 e2> | <c e> <e g> | <e g> <d fis> | d2 es es4 c4 |
	<d1 e1> <d2 f2> | <g2 b2> f2 |
}

themeBeforeBreak1EndRightHand = \notes \relative c' {
	 <cis2 e2> <e2 g2> | <d2 f2> <c4 f4> cis4 <dis4 fis4> r4 |
}

themeFinalEndRightHand = \notes \relative c' {
	 <cis2 e2> <e2 g2> | <d1 f1>
}

themeLeftHand =	\notes \relative c {
	< \context Voice = one { \stemup
		h'2 | h c | cis c | a4 c c2 b4 a ~ | a1 \breathe
		b2 | e d | h cis | b a | h \breathe

		g | a c | cis c | a4 c c2 b4 a ~ | a1 \breathe
		b2 | e d |
	  }
          \context Voice = two { \stemdown
		<d,2 g> | <f a> g ~ | g c | g a g | <a,1 fis'>
		<d2 g> | <g b> <f a> | a a | g a | h

		g | g g ~ | g c | g a g | <a,1 fis'>
		<d2 g> | <g b> <f a> |
	  }
	>
}

themeBeforeBreak1EndLeftHand = \notes \relative c' {
	 <a2 h2> <a2 cis2> | <g2 b2> a2 h4 r4 |
}

themeFinalEndLeftHand = \notes \relative c' {
	 <a2 h2> <a2 cis2> | <g1 b1>
}

themePedal = \notes \relative c' {
	g4 fis | f2 c | a' d, |
	h c es | d ~ d \breathe b |
	e, g | a a | d, f4 a | h2 \breathe

	c2 | e2 c | a' d, |
	h c es | d ~ d \breathe b |
	e, g |
}

themeBeforeBreak1EndPedal = \notes \relative c {
	a2 a | d,2 f4 a h4 r4 |
}

themeFinalEndPedal = \notes \relative c {
	a2 a | d,1_\fermata
}

break1Melody = \notes \relative c'' {
	g4( fis e fis d' h | )a1. |
	g4( es g b es g | <fis1. )a1.> |
	b,2 g es | d1. |
	b'2 g d' | a1 r2 |
}

break1RightHand = \notes \relative c' {
	e1. | g4( fis e fis g fis |
	)es1. | dis'2 d4 c b a \breathe |
	s1. | s1. |
	es1 <es2 g2> | <dis1 fis1 > r2 |
}

break1LeftHand = \notes \relative c {
	<
		\context Voice = oben {
			\stemup
			c'1. | d2. c4 h a | b1. | d1. |
		}
		\context Voice = unten {
			\stemdown
			<c,1. g'1.> | <d1. a'1.> | <es1. g1.> | d2. e4 fis d |
		}
	>
	r4 b2( g4 b c | d c b g fis d |
	<)es1 c'1> <c2 c'2> | <h1 h'1> r2 |
}

break2RightHand = \notes \relative c'' {
	g8 r8 c,8 r8 <fis8 g8> r8 c r g' r c, r |
	<fis8 g8> r  c r  b' r  es, r  <es g> r  es  r |
	b' r es, r <es g> r es r d' r <d, g> r |
	d' r f, r <f b> r f r d' r <fis, a> r |
	d' r g, r <g b> r g r d' r g, r |
	es' r g, r <g b> r g b es r <a, c> r |
	d8 r b r d8 f d b a4-. <a-. cis-.> |
	<fis1 a d1> \breathe
}

break2LeftHand = \notes \relative c' {
	r8 g r g r g r g r g r g |
	r8 g r g r b r b r b r b |
	r c r c r c r c r h r h |
	r d r d r d r d r d r d |
	r d r d r d r d r c r c |
	r <b es> r <b es> r <b es> r <g es'> r f' r <a, f'> |
	r <b f'> r <b f'> r4 <g4-. f'-.> <a4-. e'-.> <a-. e'-.> |
	<d,1 d'1> \breathe
}

break2Pedal = \notes \relative c {
	c2 c g' | es1 d2 | c1 h2 |
	b1 d2 \breathe | g2 g f2 | es1 f2 |
	b,2 g a | d1 \breathe
}

\score {
	<
		\context PianoStaff <
			\property PianoStaff.instrument = "Manual"
			\context Staff = treble <
				\global
				\context Voice = melody {
					\stemup \themeMelody \themeBeforeBreak1EndMelody
					\break1Melody
					\stemboth \break2RightHand
					\stemup \themeMelody \themeFinalEndMelody
				}
				\context Voice = righthand {
					\stemdown \themeRightHand \themeBeforeBreak1EndRightHand
					\break1RightHand
					\notes {
						s1. | s1. | s1. | s1. |
						s1. | s1. | s1. | s1
					}
					\themeRightHand \themeFinalEndRightHand
				}
			>
			\context Staff = bass <
				\clef "bass";
				\global
				\context Voice = lefthand {
					\themeLeftHand \themeBeforeBreak1EndLeftHand
					\break1LeftHand
					\break2LeftHand
					\themeLeftHand \themeFinalEndLeftHand
				}
			>
		>
		\context Staff = pedal {
			\property Staff.instrument = "Pedal"
			\clef "bass";
			<
				\global
				\context Voice = pedal {
					\themePedal \themeBeforeBreak1EndPedal
					\notes {
						R1. R1. R1. R1. R1. R1. R1. R1.
					}
					\break2Pedal
					\themePedal \themeFinalEndPedal
				}
			>
		}
	>

	\header {
		tagline = "";
	}

	\paper {
		linewidth = 18.8 \cm;
		textheight = 28.0 \cm;

		\translator { \OrchestralScoreContext }
%		\translator { \StaffContext \consists "Staff_margin_engraver"; }
%		\translator { \PianoStaffContext \consists "Staff_margin_engraver"; }
	}
}
