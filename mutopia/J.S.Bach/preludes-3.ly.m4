\header{
filename =	 "preludes-3.ly";
title =	 "3";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}

\include "paper20.ly"

define(comma, r16 [$1$4 $2 $3] [$2 $1 $2 $6$1] r $1$7 r $5$1 |)
define(s, `comma(translit($*,` ', `,'))')
define(t, r16 [$1$4 $2 $3] [$2 $1 $2 $6$1] r $1$7 r $5$1 |)

rh = \melodic{
	\octave c';
	\textstyle "italic";
%#	s(`c' `es' `g' `\p(\<' `\!' `)\!' `\>')
	t(c,es,g,`\p(\<',\!,`)\!',\>)
%#	s(c es g `(' `' `)')
	t(c,es,g,`(',`',`)')
%#	s(c f as `(' `' `)')
	t(c,f,as,`(',`',`)')
	s(c f as)
	s('b d f \< \!)
	s('b d f)
	s('g c es \> \!)
	s('g c es)
	s('g c es \< \!)
	s('g c es)
	s('a c es)
	s('a c es)
	s('a 'bes d \> \!)
	s('g 'bes d)
	s('bes d g)
	s('a es g \mf)
	s('a c fis)
	s('a c fis)
	s('bes d g)


	s(c fis a -"poco a poco cresc.")
	s(d g bes)
	s(d fis c')

	s(cis g bes)
	s(c es a !-"dim.")
	s('bes e g)
	s('a c g)

	s('a c fis)
	s('g 'bes e)
	s('fis 'a es)

	s('g 'bes d -"dim.")
	s('g 'a c)
	s('fis 'a c)
	s('fis 'a c)
	r16\p ['g 'a c] ['b 'g 'b 'g] r 'g r 'g
	s('a c fis -"poco cresc.")
	s(c fis a)
	s('b d g)
	s('b d f -"dim.")
	s('g c es)
	s('fis c es)
	s('fis c es)
	r16 ['g(\< 'b d] ['b 'g 'b d] [es c 'a )\!fis] |
	\multi 2 <
		{\stemup; g2.-\fermata\mf}
		{\stemdown; <'b2. d>}
	>
	\stemboth;
	\bar "|.";
}

lh = \melodic{
	\octave c;
	\clef bass;
	% c4 r [g8-.(\ped )es-.\*] |
	c4 r [g8-.( )es-.] |
	c4 r [g8-.( )es-.] |
	c4 r [as8-.( )f-.] |

	c4 r [as8-.( )f-.] |
	c4 r [as8-.( )f-.] |
	c4 r [as8-.( )f-.] |

	c4-- r [es8 c] |
	'bes!4-- r [es8 c] |
	'as!4-- r [es8 c] |

	'g4-- r [es8 c] |
	'fis4-- r [es8 c] |
	'fis4 r [fis8 d] |

	'g4 r [d8 'bes] |
	'g4 r ['bes8 'g] |
	'es4 r [g8 es] |

	c4 r [c8 'a] |
	'd4 r [d8 'a] |
	'd4 r [d8 'a] |
	'd4 r [d8 'bes] |


	'd4 r [es8 c] |
	'd4 r [g8 d] |
	'd4 r [a8 fis] |

	'd4 r [g8 es] |
	'd4 r [fis8 d] |
	'd4 r [e8 cis] |
	'd4 r [es8 c] |

	'd4 r [d8 'a] |
	'd4 r [cis8 'bes] |
	'd4 r [c!8 'a] |

	'd4 r ['bes8 'g] |
	'd4 r [es8 c] |
	'd4 r [d8 c] |

	'g4 r [es8 c] |
	'g4 r [d8 'b] |
	'g4 r [es8 c] |
	
	'g4 r [es8 c] |
	'g4 r [g8 d] |
	'g4 r [as8 f] |
	'g4 r [es8 c] |

	'g4 r [es8 c] |
	'g4 r [es8 c] |
	'g4 r r |
	'g2._\fermata 
	\bar "|.";
}

global  = \melodic{
	\meter 3/4;
	\key bes es as;
}

\score{
	% Semplice e non troppo legato
	\melodic \type Grandstaff < 
		<
			\global 
			\rh
		>
		<
			\global
			\lh
		>
	>
	\paper{
		\paper_twenty
		linewidth= 195.\mm;
	}
	\midi{
		\tempo 4 = 90;
	}
}
