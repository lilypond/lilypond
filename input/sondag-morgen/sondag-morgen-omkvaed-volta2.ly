\version "1.5.68"
\include "deutsch.ly"

marks = \lyrics {
    \marks
    ""1*9
}

ned = \downbow
op = \upbow

global = \notes {
    \global
    \bar "|."
    \property Score.repeatCommands = #'((volta #f)(volta "2"))
    s1*9
}

akk = \lyrics {
    \akk
    "A11"2 "H11"
    "Em7" "G$\\Delta$" "A11" "H11"
    "C$\\Delta$"1  "D6" "Am9" "Hm7" "C$\\Delta$"1  "H9$\\sharp$"
}


mel = \notes {
    \mel
    \relative g' {
	r2 r8 e'4 d8 ~ |
	d8 h4 e8~e h g a ~|
	a g a g a g a g |
	h4. e,8~e2 |
	h'4. e,8~e2 |
	h'4. e,8~e2 |
	h'4. d8~d4 c |
	h4. e,8~e2 |
	h'4 h r8 e4 d8 |
    }
}

kormel = \notes {
    \kormel
    \relative c'' {
	R1*2
	r2 r8 e e e
	\repeat unfold 5 { e4. e8~e2 }
	cisis4 cisis r2
    }
}

sopsax = \notes {
    \sopsax
    \context Voice=sax \relative c'' {
    a8 h4 d8 r d4 e8-\sfz |
    r4 g,8-\mf r r fis r4 |
    r8 d r4 r8 e r4 |
    s1*0-\mp
    \repeat unfold 2{
	g4.-. g8 \< ~g4 \! r4*1/2 \> \! s4*1/2 |
	a4.-. a8 \< ~a4 \! r4*1/2 \> \! s4*1/2  |
    }
    g4.-. g8 \< ~g4 \! r4*1/2 \> \! s4*1/2 |
    cisis4-\ff cisis8 d r dis4 e8 | 
    }
}
altsax = \notes {
    \altsax
    \context Voice=sax \relative c'' {
    a8 h4 d8 r d4 e8-\sfz |
    r4 e,8-\mf r r d r4 |
    r8 h r4 r8 cis r4 |
    s1*0-\mp
    \repeat unfold 2{
	e4.-. e8 \< ~e4 \! r4*1/2 \> \! s4*1/2 |
	fis4.-. fis8 \< ~fis4 \! r4*1/2 \> \! s4*1/2  |
    }
    e4.-. e8 \< ~e4 \! r4*1/2 \> \! s4*1/2 |
    a4-\ff a8 d r dis4 e8 |
    }
}
tensax = \notes {
    \tensax
    \context Voice=sax \relative c' {
    a8 h4 d8 r d4 e8-\sfz |
    r4 h8-\mf r r a r4 |
    r8 g r4 r8 a r4 |
    s1*0-\mp
    \repeat unfold 2{
	d4.-. d8 \< ~d4 \! r4*1/2 \> \! s4*1/2 |
	e4.-. e8 \< ~e4 \! r4*1/2 \> \! s4*1/2  |
    }
    d4.-. d8 \< ~d4 \! r4*1/2 \> \! s4*1/2 |
    dis4-\ff dis8 d r dis4 e8 | 
    }
}

piar = \notes {
    \piar
    \relative c'' {
	r8 <g,4 a h d e> <a8 h cis e> r8 <d4 a' d> <e8 h' e> |
	s1*2
	<g,1-\mf \< h d e g>
	<a d e fis>
	<g c d e g>
	<a d e fis>
	<g1 \! h d e g> |
	<dis'4.-\ff fis a h cisis> <d8 a' d> r8 <dis4 ais' dis> <e8 h' e> |
    }
}
U = \sustainUp
D = \sustainDown
pial = \notes {
    \pial
    a,,8 a, e, <h,,8 h,> r <d,,4 d,> e,8 | 
    e,,8 e, \su <g h d' e' g'> \sd g, g,, \su <g a h d' fis'> \sd e, a,
    a,, \su <g a h d' e' > \sd e, h, h,, \su <a h cis' e'> \sd d, e, |
    <c,1 c>
    <d, d>
    <a,, a,>
    <h,, h,>
    <c, c>
    <h,,4. h,> <d,8 d> r <dis,4 dis> <d,8 d> |
}
guitar = \notes {
    \guitar
    e8-2 e-2 < a-3 d'-4> e-2 e-2 < a-3 d'-4> e-2 < h-3 e'-4> |
    e8-2 < d'-4 g'-5> e-2 e-2 < e'-4 a'-5> e-2 < d'-4 g'-5> e-2  |
    e-2 e-2 < a-3 d'-4> e-2 e-2 e-2 < h-3 e'-4> e-2 |
    s1*0_"FLANGE"
    \repeat unfold 2 {
	r8 d'-4 <h'-6 g'-5> d'-4 a'-5 d'-4 e'-4 d'-4
	r8 d'-4 <h'-6 fis'-5> d'-4 a'-5 d'-4 e'-4 d'-4
    }
    r8 d'-4 <h'-6 g'-5> d'-4 a'-5 d'-4 e'-4 d'-4
    <h4-3_"NO FLANGE" dis'-4 a'-5 cisis''-6> r8 < a-3 d'-4> e-2 < ais-3 dis'-4> e-2 < h-3 e'-4> |
}

cello = \notes {
    \cello
    R1*9
}

bas = \notes {
    \bas
    a,8 a, r h, r d4 e8 |
    e,8 e, r8 g, g, g, r a, |
    a, a, r h, h, h, r4 |
    c4. c'8 r c'4.
    d4. d'8 r d'4.
    a,4. a8 r a4.
    h,4. h8 r h4.
    c4. c'8 r c'4.
    h,4 h,8 d8 r dis4 e8 |
}

\include "drumpitch-init.ly"
drhi = \notes {
    \drhi
    hh8 hh hh hho r cymc4 cymc8 |
    hh8 hh hh hh hh hh hh hh |
    hh8 hh hh hh hh hh hh hho |
    \repeat percent 5 { hh8 hh hh hho hh hho hh hh16 hh }
    hh8 hh hho cymc8~cymc cymc4 cymc8 |
}
drlo = \notes {
    \drlo
    bd8 sn bd bd hhp bd4 bd8 |
    bd4 sn8 bd bd4 sn8 bd |
    bd4 sn8 bd bd4 < 
	bd sn
    > |
    \repeat percent 5 { bd4 bd8 bd bd4 bd } |
    bd8 sn bd bd r bd4 bd8 |
}
