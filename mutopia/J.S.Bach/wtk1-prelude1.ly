\header{
filename =	 "prelude1.ly";
title =	 "Preludium";
description =	 "Das Wohltemperierte Clavier I, Prelude I (c-major)";
opus =	 "BWV 846";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "Shay Rojansky";
copyright =	 "Public Domain";
}

\version "1.0.7";

global = 
    \notes {
	 \time 4/4;                % should be \time C
    }
  

% should do programmable input.

soprane =
    \notes \transpose c'' {
        \clef "violin";

	\type Staff <
	% Real soprane
	{\stemup
	r8 [g16 c'] [e' g c' e'] r8 [g16 c'] [e' g c' e'] |
	r8 [a16 d'] [f' a d' f'] r8 [a16 d'] [f' a d' f'] |
	r8 [g16 d'] [f' g d' f'] r8 [g16 d'] [f' g d' f'] |
	r8 [g16 c'] [e' g c' e'] r8 [g16 c'] [e' g c' e'] |
	r8 [a16 e'] [a' a e' a'] r8 [a16 e'] [a' a e' a'] |
	r8 [fis16 a] [d' fis a d'] r8 [fis16 a] [d' fis a d'] |
	r8 [g16 d'] [g' g d' g'] r8 [g16 d'] [g' g d' g'] |
	r8 [e16 g] [c' e g c'] r8 [e16 g] [c' e g c'] |
	r8 [e16 g] [c' e g c'] r8 [e16 g] [c' e g c'] |}
	% Tenor
	{\stemdown
	r16 e8. ~ e4 r16 e8. ~ e4 |
	r16 d8. ~ d4 r16 d8. ~ d4 |
	r16 d8. ~ d4 r16 d8. ~ d4 |
	r16 e8. ~ e4 r16 e8. ~ e4 |
	r16 e8. ~ e4 r16 e8. ~ e4 |
	r16 d8. ~ d4 r16 d8. ~ d4 |
	r16 d8. ~ d4 r16 d8. ~ d4 |
	r16 c8. ~ c4 r16 c8. ~ c4 |
	r16 c8. ~ c4 r16 c8. ~ c4 |}>
%% 10	
	r8 [d16 fis] [c' d fis c'] r8 [d16 fis] [c' d fis c'] |
	r8 [d16 g] [b d g b] r8 [d16 g] [b d g b] |
	r8 [e16 g] [cis' e g cis'] r8 [e16 g] [cis' e g cis'] |
	r8 [d16 a] [d' d a d'] r8 [d16 a] [d' d a d'] |
	r8 [d16 f] [b d f b] r8 [d16 f] [b d f b] |
	r8 [c16 g] [c' c g c'] r8 [c16 g] [c' c g c'] |
	r8 [a,16 c] [f a, c f] r8 [a,16 c] [f a, c f] |
	r8 [a,16 c] [f a, c f] r8 [a,16 c] [f a, c f] |
	r8 [g,16 b,] [f g, b, f] r8 [g,16 b,] [f g, b, f] |
	r8 [g,16 c] [e g, c e] r8 [g,16 c] [e g, c e] |
%% 20
	r8 [bes,16 c] [e bes, c e] r8 [bes,16 c] [e bes, c e] |
	r8 [a,16 c] [e a, c e] r8 [a,16 c] [e a, c e] |
	r8 [a,16 c] [ees a, c ees] r8 [a,16 c] [ees a, c ees] |
	r8 [b,16 c] [d b, c d] r8 [b,16 c] [d b, c d] |
	r8 [g,16 b,] [d g, b, d] r8 [g,16 b,] [d g, b, d] |
	r8 [g,16 c] [e g, c e] r8 [g,16 c] [e g, c e] |
	r8 [g,16 c] [f g, c f] r8 [g,16 c] [f g, c f] |
	r8 [g,16 b,] [f g, b, f] r8 [g,16 b,] [f g, b, f] |
	r8 [a,16 c] [fis a, c fis] r8 [a,16 c] [fis a, c fis] |
	r8 [g,16 c] [g g, c g] r8 [g,16 c] [g g, c g] |
%% 30
	r8 [g,16 c] [f g, c f] r8 [g,16 c] [f g, c f] |
	r8 [g,16 b,] [f g, b, f] r8 [g,16 b,] [f g, b, f] |
	r8 [g,16 bes,] [e g, bes, e] r8 [g,16 bes,] [e g, bes, e] |

	r8 [f,16 a,] [c f c a,] [c a, f, a,] [f, d, f, d,] |
	r8 [g16 b] [d' f' d' b] [d' b g b] [d f e d] |
	<e1 g c'> ||
    }

bass =
    \notes {
        \clef "bass";


	\stemdown
	c'2 c' |
	c' c' |
	b b |
	c' c' |
	c' c' |
	c' c' |
	b b |
	b b |
	a a |	
%% 10
	\type Staff <
	% Alt
	{\stemup
	r16 a8. ~ a4 r16 a8. ~ a4 |
	r16 b8. ~ b4 r16 b8. ~ b4 |
	r16 bes8. ~ bes4 r16 bes8. ~ bes4 |
	r16 a8. ~ a4 r16 a8. ~ a4 |
	r16 aes8. ~ aes4 r16 aes8. ~ aes4 |
	r16 g8. ~ g4 r16 g8. ~ g4 |
	r16 f8. ~ f4 r16 f8. ~ f4 |
	r16 f8. ~ f4 r16 f8. ~ f4 |
	r16 d8. ~ d4 r16 d8. ~ d4 |
	r16 e8. ~ e4 r16 e8. ~ e4 |
%% 20
	r16 g8. ~ g4 r16 g8. ~ g4 |
	r16 f8. ~ f4 r16 f8. ~ f4 |
	r16 c8. ~ c4 r16 c8. ~ c4 |
	r16 f8. ~ f4 r16 f8. ~ f4 |
	r16 f8. ~ f4 r16 f8. ~ f4 |
	r16 e8. ~ e4 r16 e8. ~ e4 |
	r16 d8. ~ d4 r16 d8. ~ d4 |
	r16 d8. ~ d4 r16 d8. ~ d4 |
	r16 ees8. ~ ees4 r16 ees8. ~ ees4 |
	r16 e!8. ~ e4 r16 e8. ~ e4 |
%% 30
	r16 d8. ~ d4 r16 d8. ~ d4 |
	r16 d8. ~ d4 r16 d8. ~ d4 |
	r16 c8. ~ c4 r16 c8. ~ c4 |

	r16 c8. ~ c4 ~ c2 |
	r16 b,8. ~ b,4 ~ b,2 |
	c1 ||}

	% Bass
	{\stemdown
	d2 d |
	g g |
	g g |
	f f |
	f f |
	e e |
	e e |
	d d |
	g, g, |
	c c |
%% 20
	c c |
	f, f, |
	fis, fis, |
	aes, aes, |
	g, g, |
	g, g, |
	g, g, |
	g, g, |
	g, g, |
	g, g, |
%% 30
	g, g, |
	g, g, |
	c, c, |
	
	c, c, |
	c, c, |
	c,1 }>
    }

        
\score {
    \notes \type GrandStaff < 
                  <
                       \global 
                       \soprane
                 >
                  <
                       \global
		       \bass
                 >
             >

    \paper{
	gourlay_maxmeasures =5.;	
    }
    \midi {
        \tempo 4 = 84;
    }
}

% EOF
