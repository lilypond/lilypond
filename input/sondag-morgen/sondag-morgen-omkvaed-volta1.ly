\version "1.5.68"
\include "deutsch.ly"

marks = \lyrics {
    \marks
    ""1*5
}

ned = \downbow
op = \upbow

global = \notes {
    \global
    \property Score.repeatCommands = #'((volta "1"))
    s1*4 s2..
    \mark #'(rows "D. S.  " (music "scripts-segno"))
    s8
}

akk = \lyrics {
    \akk
    "A11"2 "H11"
    "Em7"  "G$\\Delta$" "Am7"1 "Dsus4" "D"
}


mel = \notes {
    \mel
    \relative g' {
	r2 r8 e'4 d8 ~ |
	d8 h4 e8~e h g a ~|
	a1 ~ |
	a1 ~ |
	a2 r8 d, g a
    }
}

kormel = \notes {
    \kormel
    \relative c'' {
	r2 r8 e4 d8~
	d h4 e8~e d h d~
	d1~d1~d2 r2
    }
}

sopsax = \notes {
    \sopsax
    \context Voice=sax \relative c'' {
	a8 h4 d8 r d4 e8-\sfz |
	r4 g,8-\mf r r fis r8 e8~ |
	e4. g'8->-\f ~g \> g e4 |
	g-> a8 d, ~ d4 \! d |
	d2-\p r |
    }
}
altsax = \notes {
    \altsax
    \context Voice=sax \relative c'' {
	a8 h4 d8 r d4 e8-\sfz |
	r4 e,8-\mf r r d r8 c |
	~ c4. d'8->-\f ~d8 \> d c4 |
	d4 e8 d ~ d4 \! h |
	a2-\p r |
    }
}
tensax = \notes {
    \tensax
    \context Voice=sax \relative c' {
	a8 h4 d8 r d4 e8-\sf |
	r4 h8-\mf r r a r g |
	~ g4. a'8->-\f ~ a \> a g4 |
	a4-> a8 a ~ a4 \! g |
	fis2-\p r|
    }
}

piar = \notes {
    \piar
    \relative c'' {
	r8 <g,4 a h d e> <a8 h cis e> r8 <d4 a' d> <e8 h' e> |
	r2 r4 r8 <g, c e > ~|
	<g1 c e > |
	<d' e g a>|
	<a2 d fis> r8 d g a |
    }
}
U = \sustainUp
D = \sustainDown
Su = { \su \stemDown }
Sd = { \sd \stemUp }
pial = \notes {
    \pial
    \stemBoth a,, a, e, <h,,8 h,> r <d,,4 d,> e,8 | 
    e,,8 e, \Su <g h d' e' g'> \Sd g, g,, \Su <g a h d' fis'> \Sd e, <a,, a,>~
    \stemBoth <a,,1 a,> |
    <d, d> |
    <d,2 d> r |
}
guitar = \notes {
    \guitar
    e8-2 e-2 < a-3 d'-4> e-2 e-2 < a-3 d'-4> e-2 < h-3 e'-4> |
    e8-2 < d'-4 g'-5> e-2 e-2 < e'-4 a'-5> e-2 e-2 <g8-3 c'-4 e'-5 a'-6> ~ |
    <g1-3 c'-4 e'-5 a'-6>
    <a1-3 d'-4 e'-5 a'-6> ~
    <a2-3 d'-4 fis'-5 a'-6> r |
}

cello = \notes {
    \cello
    R1*5
}

bas = \notes {
    \bas
    a,8 a, r h, r d4 e8 |
    e,8 e, r8 g, g, g, r a,~ |
    a,2. ~ a,4 \glissando |
    d1 |
    r2 d4 d |
}

\include "drumpitch-init.ly"
drhi = \notes {
    \drhi
    hh8 hh hh hho r cymc4 cymc8
    hh8 hh hh hh hh hh hh cymc ~
    cymc4. tommh8 r tommh toml4  |
    cymr4. cymr8~cymr4 cymr4 |
    cymr2 hhc4 hho |
}
drlo = \notes {
    \drlo
    bd8 sn bd bd hhp bd4 bd8 |
    bd4 sn8 bd bd4 sn8 bd |
    bd4. bd8 r bd bd4 |
    bd4. bd8 r2 |
    r2 bd4 <
	bd8 sn
    > sn  |
}
