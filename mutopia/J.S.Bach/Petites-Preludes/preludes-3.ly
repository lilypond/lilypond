%{

 Six Petits Preludes,
 Collection Johann Peter Kellner
 ca 1703 - 1707



 Kellner was a student of Bach's.  
 
%}
\header{
  copyright =	 "public domain";
  source = "Ed. Henry Lemoine Urtext";
  title =	 "Pr\\\"aludum in C moll";
  subtitle = "f\\\"ur Laute";
  composer =	 "Johann Sebastian Bach (1685-1750)";
  enteredby =	 "jcn,hwn";
  copyright =	 "public domain";

  % mutopia headers.
  mutopiatitle = "Prelude";
  mutopiacomposer = "J.S.Bach";
  mutopiaopus = "BWV999";
  mutopiainstrument = "Piano";
  style = "baroque";
  copyright = "Public Domain";
  tagline =    "\\\\This music is part of the Mutopia project, http://sca.uwaterloo.ca/Mutopia/\\\\It has been typeset and placed in the public domain by Jan Nieuwenhuizen.\\\\Unrestricted modification and redistribution is permitted and encouraged - copy this music and share it!";
  maintainer = "janneke@gnu.org";
  lastupdated = "1999/Oct/16";
  
}
%{

TODO: this file needs additional layouting: the upper voice should
switch staffs to avoid leger lines.

%}
\version "1.3.4";


upper =  \notes \transpose c'' {
	r16 [c es g] [es c es c] r c r c |
	r16 [c es g] [es c es c] r c r c |
	r16 [c f as] [f c f c] r c r c |
	r16 [c f as] [f c f c] r c r c |
	r16 [b, d f] [d b, d b,] r b, r b, |
	r16 [b, d f] [d b, d b,] r b, r b, |
	r16 [g, c es] [c g, c g,] r g, r g, |
	r16 [g, c es] [c g, c g,] r g, r g, |
	r16 [g, c es] [c g, c g,] r g, r g, |
	r16 [g, c es] [c g, c g,] r g, r g, |
	r16 [a, c es] [c a, c a,] r a, r a, |
	r16 [a, c es] [c a, c a,] r a, r a, |
	r16 [a, bes, d] [bes, a, bes, a,] r a, r a, |
	r16 [g, bes, d] [bes, g, bes, g,] r g, r g, |
	r16 [bes, d g] [d bes, d bes,] r bes, r bes, |
	r16 [a, es g] [es a, es a,] r a, r a, |
	r16 [a, c fis] [c a, c a,] r a, r a, |
	r16 [a, c fis] [c a, c a,] r a, r a, |
	r16 [bes, d g] [d bes, d bes,] r bes, r bes, |

	r16 [c fis a] [fis c fis c] r c r c |
	r16 [d g bes] [g d g d] r d r d |
	r16 [d fis c'] [fis d fis d] r d r d |

	r16 [cis g bes] [g cis g cis] r cis r cis |
	r16 [c! es a] [es c es c] r c r c |
	r16 [bes, e g] [e bes, e bes,] r bes, r bes, |
	r16 [a, c g] [c a, c a,] r a, r a, |

	r16 [a, c fis] [c a, c a,] r a, r a, |
	r16 [g, bes, e] [bes, g, bes, g,] r g, r g, |
	r16 [fis, a, es] [a, fis, a, fis,] r fis, r fis, |
	r16 [g, bes, d] [bes, g, bes, g,] r g, r g, |
	r16 [g, a, c] [a, g, a, g,] r g, r g, |
	r16 [fis, a, c] [a, fis, a, fis,] r fis, r fis, |
	r16 [fis, a, c] [a, fis, a, fis,] r fis, r fis, |
	r16 [g, a, c] [b, g, b, g,] r g, r g,
	r16 [a, c fis] [c a, c a,] r a, r a, |
	r16 [c fis a] [fis c fis c] r c r c |
	r16 [b, d g] [d b, d b,] r b, r b, |
	r16 [b, d f] [d b, d b,] r b, r b, |
	r16 [g, c es] [c g, c g,] r g, r g, |
	r16 [fis, c es] [c fis, c fis,] r fis, r fis, |
	r16 [fis, c es] [c fis, c fis,] r fis, r fis, |
	r16 [g, b, d] [b, g, b, d] [es c a, fis] |
	\context Staff <
		\context Voice=i {\stemup g2.-\fermata\mf}
		\context Voice=ii {\stemdown <b,2. d>}
	>
	\stemboth
	\bar "|.";
}

lower =  \notes{
	c4 r [g8 es] |
	c4 r [g8 es] |
	c4 r [as8 f] |

	c4 r [as8 f] |
	c4 r [as8 f] |
	c4 r [as8 f] |

	c4-- r [es8 c] |
	bes,!4-- r [es8 c] |
	as,!4-- r [es8 c] |

	g,4-- r [es8 c] |
	fis,4-- r [es8 c] |
	fis,4 r [fis8 d] |

	g,4 r [d8 bes,] |
	g,4 r [bes,8 g,] |
	es,4 r [g8 es] |

	c4 r [c8 a,] |
	d,4 r [d8 a,] |
	d,4 r [d8 a,] |
	d,4 r [d8 bes,] |


	d,4 r [es8 c] |
	d,4 r [g8 d] |
	d,4 r [a8 fis] |

	d,4 r [g8 es] |
	d,4 r [fis8 d] |
	d,4 r [e8 cis] |
	d,4 r [es8 c] |

	d,4 r [d8 a,] |
	d,4 r [cis8 bes,] |
	d,4 r [c!8 a,] |

	d,4 r [bes,8 g,] |
	d,4 r [es8 c] |
	d,4 r [d8 a,] |

	g,4 r [es8 c] |
	g,4 r [d8 b,] |
	g,4 r [es8 c] |
	
	g,4 r [es8 c] |
	g,4 r [g8 d] |
	g,4 r [as8 f] |
	g,4 r [es8 c] |

	g,4 r [es8 c] |
	g,4 r [es8 c] |
	g,4 r r |
	g,2._\fermata 
	\bar "|.";
}

global  = \notes{
	\time 3/4;
	\key es;
}

\score{
	\context PianoStaff <
		\context Staff = up <
			\global
			\upper
		>
		\context Staff = down <
			\global
			\clef "bass";
			\lower
		>
	>
	\paper{
		\translator { \PianoStaffContext
			minVerticalAlign = 2.2 * \staffheight; 
			maxVerticalAlign = 2.2 * \staffheight ;
		}
		\translator {\OrchestralScoreContext }
	}
	\midi{ \tempo 4 = 100; }
	\header{
		opus = 	"BWV 999";
	}
}
