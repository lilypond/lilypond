\version "1.5.68"
\include "deutsch.ly"

marks = \lyrics {
    \marks
    "\large\framebox{\bf{VERS}}"4*54
}

ned = \downbow
op = \upbow

#(define meter '(rows (music "noteheads-2" ((kern . -0.1) "flags-stem")) " = 125"))

global = \notes {
    \global
    \mark #'(music "scripts-segno")
    \bar "|:"
    s4*54
}

akk = \lyrics {
    \akk
    "Gm7"4. "Dm7"2
    "Cm7"4. ""4 "Dm7"4
    "Gm7"4. "Dm7"2
    "Cm7"4. ""4 "Dm7"4
    "Gm7"4. "Dm7"2
    "Cm7"4. ""4 "Dm7"4
    "E$\\flat\\Delta$"4. "F6"2
    "D11"8*7 "D/fis"4
    "G"4. "D"2
    "C$\\Delta$"4. ""4 "D"
    "G"4. "D"2
    "C$\\Delta$"4. ""4 "D"
    "Em7"4. "Hm7"2
    "Am9"4. ""4 "Hm7"
    "C$\\Delta$"2 "Am9"
}


mel = \notes {
    \mel
    \time 7/8
    \key g \minor
    \relative c'' {
	[b8 a g] a4 [g8 f] |
	g4 d8 g4 a |
	b4 a8 [g a] [g f] |
	g4. r8 g a4 |
	[b8 a g] a4 [g8 f] |
	g4 d8 g4 a |
	[b8 a g] a4 [g8 f]|
	\time 9/8
	g4. r4 r8 d [g a] |
	\time 7/8
	\key g \major
	[h8 a g] a4 d |
	h g8 g4 a |
	h4 a8 [g a] [h d] |
	g,4. r8 g a4 |
	[h8 a g] a4 [h8 d] |
	h4 g8 g4 a |
	\time 4/4
	\repeat unfold 2 \times 2/3 { h4 a g  }|
    }
}

kormel = \notes {
    \kormel
    \time 7/8
    \relative c' {
	R1*7/8*5 |
	r4 r8 b'4 c |
	[d8 c b] c4 [b8 c] |
	\time 9/8
	d4. R4 R4 R4
	\time 7/8
	R1*7/8*5 |
	r4 r8 h4 c |
	\time 4/4
	\repeat unfold 2 \times 2/3 {d4 c h}
    }
}

sopsax = \notes {
    \sopsax
    \context Voice=sax \relative c'' {
	\key g \minor
	\time 7/8
	% s1*0_"1 $\\times$ tacet"
	R1*7/8
	r4. g4-\mf a |
	b4. r2 |
	R1*7/8*2
	r4. b4 c |
	d4. \< d4 d |
	\time 9/8
	e4.~ \! e4 r4 d |
	\time 7/8
	\key g \major
	\repeat percent 3 {
	    h4.-\mf a4 d |
	    g,4. g4 a |
	}
	\time 4/4
	\tupletUp
	\times 2/3 { h4 \<a g } \times 2/3 { h4 a \! g }
    }
}
altsax = \notes {
    \altsax
    \context Voice=sax \relative c' {
	\key g \minor
	\time 7/8
	s1*0_"1 $\\times$ tacet" R1*7/8
	r4. es4-\mf f |
	g4. r2 |
	R1*7/8*2
	r4. g4 a |
	b4. \< c4 c |
	\time 9/8
	c4.~ \! c4 r4 a |
	\time 7/8
	\key g \major
	\repeat percent 3 {
	    g4.-\mf fis4 a |
	    e4. e4 fis |
	}
	\time 4/4
	\tupletUp
	\times 2/3 { g4 \< g g }\times 2/3 { g4 g \! g }
    }
}
tensax = \notes {
    \tensax
    \context Voice=sax \relative c' {
	\key g \minor
	\time 7/8
	s1*0_"1 $\\times$ tacet" R1*7/8
	r4. b4-\mf c |
	d4. r2 |
	R1*7/8*2
	r4. es4 f |
	g4. \< a4 a |
	\time 9/8
	g4.~ \! g4 r4 d |
	\time 7/8
	\key g \major
	\repeat percent 3 {
	    d4.-\mf d4 d |
	    c4. c4 d |
	}
	\time 4/4
	\tupletUp
	\times 2/3 { d4 \< d d }\times 2/3 { d4 d \! d }
    }
}

piar = \notes {
    \piar
    \time 7/8
    \key g \minor
    \relative c' {
	\repeat percent 3 {
	    [<d8 b'> g d] [<c a'> f][<c a'> f] |
	    [<b,8 g'> es b] [<b g'> es][<c a'> f] |
	}
	[<d8 b'> g d] [<c a'> f][<c a'> f] |
	\time 9/8
	<c4. e g>~<c4 e g>  <c e g> <d fis a> |
	\time 7/8
	\key g \major
	s1*0-\mf
	\repeat percent 3 {
	    [<d8 h'> g d] <d4 fis a> <d fis d'> |
	    [<c8 e h'> g' e] <c4 e g> <d fis a> |
	}
	\time 4/4
	<c2 e g h> <c e g h> |
    }
}
U = \sustainUp
D = \sustainDown
pial = \notes {
    \pial
    \key g \minor
    \time 7/8
    \repeat unfold 3 {
	g4.\D  d4 \U \D d4*1/2 \U \D  s8 \U
	c4.\D  c4 \U \D d4*1/2 \U \D  s8 \U
    }
    es4. \D f4 \U \D f4*1/2 \U \D s8 \U
    \time 9/8
    d4. \D ~d4 d \U \D fis4*1/2 \U \D s8 \U
    \time 7/8
    \key g \major
    \repeat unfold 2 {
	g4.\D  d4 \U \D d4*1/2 \U \D  s8 \U
	c4.\D  c4 \U \D d4*1/2 \U \D  s8 \U
    }
    e4. \D h,4 \U \D h,4*1/2 \U \D s8 \U
    a,4.\D  a,4 \U \D h,4*1/2 \U \D s8 \U |
    \time 4/4
    c2 \D a,2 \U |
}
guitar = \notes {
    \guitar
    \relative c' {
	\key g \minor
	\time 7/8
	s1*0_"NO FLANGE"
	\repeat percent 3 {
	    [<f8-4_\ned b-5\mu d-6> <f-4_\op b-5\ac d-6> <f-4_\ned b-5\mu d-6>] [<f-4_\op a-5\ac d-6> <f-4_\ned a-5\mu d-6>] [<f-4_\op a-5\mu d-6><f-4_\ned a-5\ac d-6>]|
	    [<es-4_\op g-5\mu c-6> <es-4_\ned g-5\ac c-6> <es-4_\op g-5\mu c-6>] [<es-4_\ned g-5\ac c-6> <es-4_\op g-5\mu c-6>][<f-4_\ned a-5\ac d-6> <f-4_\op a-5\mu d-6>] |
	}
	[<f8-4_\ned b-5\mu d-6> <f-4_\op b-5\ac d-6> <f-4_\ned b-5\mu d-6>] [<f-4_\op a-5\ac d-6> <f-4_\ned a-5\mu d-6>] [<f-4_\op a-5\mu d-6><f-4_\ned a-5\ac d-6>]|
	\time 9/8

	<d,4.-2 g-3 h-4 e-5 a-6> r4 [<fis'8-4_\ned a-5\ac d-6> <fis8-4_\op a-5\mu d-6>] [<fis8-4_\ned a-5\ac d-6> <fis8-4_\op a-5\mu d-6>] |
	\time 7/8
	\key g \major
	\repeat percent 3 {
	    [<g8-4_\ned h-5\mu d-6> <g-4_\op h-5\ac d-6> <g-4_\ned h-5\mu d-6>] [<fis-4_\op a-5\ac d-6> <fis-4_\ned a-5\mu d-6>] [<fis-4_\op a-5\mu d-6><fis-4_\ned a-5\ac d-6>]|
	    [<e-4_\op g-5\mu c-6> <e-4_\ned g-5\ac c-6> <e-4_\op g-5\mu c-6>] [<e-4_\ned g-5\ac c-6> <e-4_\op g-5\mu c-6>][<fis-4_\ned a-5\ac d-6> <fis-4_\op a-5\mu d-6>] |
	}
	\time 4/4
	s1*0-\mf \repeat unfold 2 <c2-3 e-4 g-5 h-6>


    }
}

cello = \notes {
    \cello
    \time 7/8
    \key g \minor
    R1*7/8*7
    \time 9/8
    R1*9/8
    \time 7/8
    \key g \major
    R1*7/8*6
    \time 4/4
    R1
}

bas = \notes {
    \bas
    \key g \minor
    \time 7/8
    g4. d4 d
    c4. c4 d 
    g4. d4 d
    c4. c4 d 
    g4. d4 d
    c4. c4 d 
    es4. f4 f
    \time 9/8
    d4.~d4 d fis
    \time 7/8
    \key g \major
    g4. d4 d
    c4. c4 d 
    g4. d4 d
    c4. c4 d
    e4. h,4 h,
    a,4. a,4 h, |
    \time 4/4
    c2 a,2 |
}

\include "drumpitch-init.ly"
drhi = \notes {
    \drhi
    \time 7/8
    cymc4 hh8 r hh r hh
    r8 hh4 hhc hho |
    \repeat percent 2 {
	hh4 hh8 r hh r hh
	r8 hh4 hhc hho |
    }
    hh4 hh8 r hh r hh |
    \time 9/8
    r8 hh4 hh hhc hho |
    \repeat percent 3 {
	hh4 hh8 r hh r hh
	r8 hh4 hhc hho |
    }
    \repeat unfold 2 \times 2/3 { hh4 hhc4 hho4 }

}
drlo = \notes {
    \drlo
    \time 7/8
    bd4 sn8 bd4 [bd8 sn] |
    bd4 sn8 bd4 [bd8 sn] |
    \repeat percent 2 {
	bd4 sn8 bd4 [bd8 sn] |
	bd4 sn8 bd4 [bd8 sn] |
    }
    bd4 sn8 bd4 [bd8 sn] |
    \time 9/8
    bd4. r4 bd [< 
	bd8 sn 
    > sn] |
    \time 7/8
    \repeat percent 3 {
	bd4 sn8 bd4 [bd8 sn] |
	bd4 sn8 bd4 [bd8 sn] |
    }
    \time 4/4
    bd4 bd bd bd |    
}
