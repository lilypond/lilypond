\include "deutsch.ly"

marks = \lyrics {
    \marks
    "\large\framebox{\bf{OMKVÆD}}"1*11
}

ned = \downbow
op = \upbow

global = \notes {
    \global
    \property Score.repeatCommands = #'((volta #f))
    \bar "|:"
    s1*11
}

akk = \lyrics {
    \akk
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$"
}


mel = \notes {
    \mel
    \relative c'' {
	\key e \minor
	d8 h4 e8~e h g a ~|
	a g a g a b a g |
	h4 e, r8 e4 e8 |
	r2
	r8 e'4 d8 ~ |
	d8 h4 e8~e h g a ~|
	a b a g a b a g |
	h2 r2 |
	r2
	r8 e4 d8 ~ |
	d8 h4 e8~e h g a ~|
	a g a g a b a g |
	h4 e, r8 e4 e8 |
    }
}

kormel = \notes {
    \kormel
    \key e \minor
    R1*11
}

sopsax = \notes {
    \sopsax
    \context Voice=sax \relative c'' {
	\key e \minor
	r4 g8-\mf r r fis r4 |
	r8 d r4 r8 e r4 |
	r4 e'->-\f d8()e r4 |
	a,8 h4 d8 r d4 e8-\sfz |
	r4 g,8-\mf r r fis r8 d8~ |
	d4. e8 ~e4. e'8->-\f |
	r e d4 g8->()e r4 |
	a,8 h4 d8 r dis4 e8-\sfz |
	r4 g,8-\mf r r fis r4 |
	r8 d r4 r8 e r4 |
	r4 e'->-\f d8()e r4 |
    }
}
altsax = \notes {
    \altsax
    \context Voice=sax \relative c'' {
	\key e \minor
	r4 e,8-\mf r r d r4 |
	r8 h r4 r8 cis r4 |
	r4 e'->-\f d8()e r4 |
	a,8 h4 d8 r d4 e8-\sfz |
	r4 e,8-\mf r r d r8 h8~ |
	h4. cis8 ~cis4. e'8->-\f |
	r e d4 d8()h r4 |
	a8 h4 d8 r dis4 e8-\sfz |
	r4 e,8-\mf r r d r4 |
	r8 h r4 r8 cis r4 |
	r4 e'->-\f d8()e r4 |
    }
}
tensax = \notes {
    \tensax
    \context Voice=sax \relative c' {
	\key e \minor
	r4 h8-\mf r r a r4 |
	r8 g r4 r8 a r4 |
	r4 e'->-\f d8()e r4 |
	a,8 h4 d8 r d4 e8-\sfz |
	r4 h8-\mf r r a r8 g8 |
	~g4. a8 ~a4. e'8->-\f |
	r e d4 g8 e r4 |
	a,8 h4 d8 r dis4 e8-\sfz |
	r4 h8-\mf r r a r4 |
	r8 g r4 r8 a r4 |
	r4 e'4->-\f d8 e r4 |
    }
}

piar = \notes {
    \piar
    \relative c' {
	\key e \minor
	s1*3
	r8 <g4 a h d e> <a8 h cis e> r8 <d4 a' d> <e8 h' e> |
	
	r2 r4 r8 <g, a h d>~|
	<g4. a h d> <a8 h cis e>~<a4. h cis e> <g8 h d e g>|
	s1
	r8 <g4 a h d e> <d'8 a' d> r <dis4 ais' dis> <e8 h' e>
	s1*3
    }
}
U = \sustainUp
D = \sustainDown
pial = \notes {
    \pial
    \key e \minor
    \clef "F_8"
    e,,8 e, \su <g h d' e' g'> \sd g, g,, \su <g a h d' fis'> \sd e, a, |
    a,, \su <g a h d' e' > \sd e, h, h,, \su <a h cis' e'> \sd d, e, |
    e,,8 e, \su <g h d' e' g'> \sd g, g,, \su <g a h d' fis'> \sd e, a, |
    a,, a, e, <h,,8 h,> r <d,,4 d,> e,8 | 

    e,,8 e, \su <g h d' e' g'> \sd g, g,, \su <g a h d' fis'> \sd e, <a,, a,>~
    <a,,4. a,> <h,,8 h,>~<h,,4. h,> e,8 |
    e,,8 e, \su <g h d' e' g'> \sd g, g,, \su <g a h d' fis'> \sd e, a,
    a,, a, a,, <d,, d,> r <dis,,4 dis,> <e,,8 e,> |

    e,,8 e, \su <g h d' e' g'> \sd g, g,, \su <g a h d' fis'> \sd e, a,
    a,, \su <g a h d' e'> \sd e, h, h,, \su <a h cis' e'> \sd d, e, |
    e,,8 e, \su <g h d' e' g'> \sd g, g,, \su <g a h d' fis'> \sd e, a,
}
guitar = \notes {
    \guitar
    \key e \minor
    \time 4/4
    e8-2 < d'-4 g'-5> e-2 e-2 < e'-4 a'-5> e-2 < d'-4 g'-5> e-2  |
    e-2 e-2 < a-3 d'-4> e-2 e-2 e-2 < h-3 e'-4> e-2 |
    e8-2 < d'-4 g'-5> e-2 e-2 < e'-4 a'-5> e-2 < d'-4 g'-5> e-2  |
    e-2 e-2 < a-3 d'-4> e-2 e-2 < a-3 d'-4> e-2 < h-3 e'-4> |
    e8-2 < d'-4 g'-5> e-2 e-2 < e'8-4 a'-5> e-2 r8 <g-3_\ned h-4 d'-5 g'-6> ~  |
    <g4.-3 h-4 d'-5 g'-6> <a8-3_\ned cis'-4 e'-5 a'-6> ~<a2-3 cis'-4 e'-5 a'-6> |
    e8-2 < d'-4 g'-5> e-2 e-2 < e'-4 a'-5> e-2 < d'-4 g'-5> e-2  |
    e-2 e-2 e-2 < a-3 d'-4> e-2 < ais-3 dis'-4> e-2 < h-3 e'-4> |
     e8-2 < d'-4 g'-5> e-2 e-2 < e'-4 a'-5> e-2 < d'-4 g'-5> e-2  |
    e-2 e-2 < a-3 d'-4> e-2 e-2 e-2 < h-3 e'-4> e-2 |
    e8-2 < d'-4 g'-5> e-2 e-2 < e'-4 a'-5> e-2 < d'-4 g'-5> e-2  |
}

cello = \notes {
    \cello
    \key e \minor
    \property Score.skipBars = ##t
    R1*11
}

bas = \notes {
    \bas
    \key e \minor
    e,8 e, r8 g, g, g, r a, |
    a, a, r h, h, h, r e |
    e,8 e, r8 g, g, g, r a, |
    a, a, r h, r d4 e8 |
    e,8 e, r8 g, g, g, r a, |
    ~a,4. h,8~h,4. e8 |
    e,8 e, r8 g, g, g, r a, |
    a, a, r d8 r dis4 e8  |
    e,8 e, r8 g, g, g, r a, |
    a, a, r h, h, h, r e |
    e,8 e, r8 g, g, g, r a, |
}

\include "drumpitch-init.ly"
drhi = \notes {
    \drhi
    hh8 hh hh hh hh hh hh hh
    hh8 hh hh hh hh hh hh hho |
    hh8 hh hh hh hh hh hh hh
    hh hh hh hho r cymc4 cymc8
    hh8 hh hh hh hh hh hh cymc ~|
    cymc4. cymc8 ~cymc4. hho8 |
    hh8 hh hh hh hh hh hh hh
    hh hh hho cymc8~cymc cymc4 cymc8 |

    hh8 hh hh hh hh hh hh hh
    hh8 hh hh hh hh hh hh hho |
    hh8 hh hh hh hh hh hh hh
 
}
drlo = \notes {
    \drlo
    bd4 sn8 bd bd4 sn8 bd |
    bd4 sn8 bd bd4 < 
	bd sn
    > |
    bd4 sn8 bd bd4 sn8 bd |
    bd8 sn bd bd hhp bd4 bd8 |
    bd4 sn8 bd bd4 sn8 bd |
    r4 r8 bd r4 r8 bd |
    bd4 sn8 bd bd4 sn8 bd |
    bd8 sn bd bd hhp bd4 bd8 |
    
    bd4 sn8 bd bd4 sn8 bd |
    bd4 sn8 bd bd4 < 
	bd sn
    > |
    bd4 sn8 bd bd4 sn8 bd |
    
}
