\version "1.5.68"
\include "deutsch.ly"

marks = \lyrics {
    \marks
    "\large\framebox{\bf{OUTRO}}"1*11
}

ned = \downbow
op = \upbow

global = \notes {
    \global
    \property Score.repeatCommands = #'((volta #f))
    \bar ".|"
    s1*11
    \bar "|."
}

akk = \lyrics {
    \akk
    "Em7"2 "G"2 "Hm7"1 "Em7" "Hm7" "Em7" "Hm7"
    "Em7"1 "Hm7" "Em7" "Hm7" "Em7"

}


mel = \notes {
    \mel
    \time 2/2
    \relative c'' {
	d8 h4 e8~e h g a ~|
	a b a g a b a g |
	e1
	R1
	g2 ~ g8 a g fis ~ |
	fis h,2.. |
	e1 |
	fis2. r4 |
	g2 ~ g8 a g fis ~ |
	fis h,2.. |
	e1 |
    }
}

kormel = \notes {
    \kormel
    \time 2/2
    \relative g' {
	R1*4
	h2.. a8 ~ |
	a d,2.. |
	g1 |
	d2. r4 |
	h'2.. a8 ~ |
	a d,2.. |
	g1 |
    }
}

sopsax = \notes {
    \sopsax
    \time 2/2
    \relative c''' {
	r2 g-\p( fis d )h1~h1
	h2.. a8~ a fis2.. |
	g2( a )a2 d |
	h2.. a8 ~ a fis2.. |
	g2 r2
    }
}
altsax = \notes {
    \altsax
    \time 2/2
    \relative c'' {
	r1 r1 r1 r1
	g2-\p ~g8 a g fis ~ fis d2..|
	e2( g )fis a |
	g2~g8 a g fis ~ fis d2..|
	e2 r
    }
}
tensax = \notes { 
    \tensax
    \time 2/2
    \relative c'' {
	r1 r1 r2 g-\p( fis d
	)e2.. d8 ~ d h2.. |
	h2( d )d d |
	e2.. d8 ~ d h2.. |
	h2 r
    }
}

piar = \notes {
    \piar
    \time 2/2
    \relative c' {
	r2 <d-\mf g h>
	<a1 d fis>
	fis'8( g d' fis, g d' fis, )g |
	<a1-\arpeggio h d e a> 
	R1
	fis8( g d' fis, g d' fis, )g |
	<d1-\arpeggio e g a d> 
	R1
	fis8( g d' fis, g d' fis, )g |
	<a1-\arpeggio h d e a>
	\clef "G^8"
	\property PianoStaff.Arpeggio \override #'arpeggio-direction = #1
	\property PianoStaff.connectArpeggios = ##t
	<h1-\arpeggio fis' d' g a d> 
    }
}
U = \sustainUp
D = \sustainDown
pial = \notes {
    \pial
    \clef "F"
    \time 2/2
    <e,2 e> <g,2 \D g>
    \relative e { 
	<h1 \U \D fis' d'>
	<e1 \U \D h'>
	<h \U \D fis' d'>
	<e1 \U \D h' g'>
	<h \U \D fis' d'>
	<e1 \U \D h'>
	<h \U \D fis' d'>
	<e1 \U \D h'>
	<h1*7/8 \U \D fis' d'> s8 \U
	<e1*1/2-\arpeggio \D h' g'> s2 \U
    }
}
guitar = \notes{
    \guitar
    \time 2/2
    r2 <d'_"FLANGE"-4 g'-5 h'-6> |
    \property Staff.Arpeggio \override #'arpeggio-direction = #1
    a8-3( d'-4 a'-5 a-3 d'-4 a'-5 a-3 )d'-4 |
    <e1-\arpeggio-2 a-3 d'-4 g'-5 h'-6>
    g'8-5 fis'4-5 d'8-4 r8 h4.-3 |
    a8-3( d'-4 a'-5 a-3 d'-4 a'-5 a-3 )d'-4 |
    <e1-\arpeggio-2 a-3 d'-4 fis'-5 h'-6>
    g'8-5 fis'4-5 d'8-4 r8 h4.-3 |
    a8-3( d'-4 a'-5 a-3 d'-4 a'-5 a-3 )d'-4 |
    <e1-\arpeggio-2 a-3 d'-4 g'-5 h'-6>
    g'8-5 fis'4-5 d'8-4 r8 h4.-3 |
    <e,1-\arpeggio-1 e-2 a-3 d'-4 g'-5 h'-6>
}

cello = \notes {
    \cello
    \time 2/2
    \relative c {
        e1 d2( fis g h d fis )e1 fis2( d
	)e1 d2( h g e fis d )e1
    }
}

bas = \notes {
    \bas
    \time 2/2
    e2 g, |
    h,2 r8 a h4 |
    e2 r8 a,4 h,8 |
    r h a fis a h g4 |
    e2 r8 d4 h,8 ~ |
    h,2 r2
    e2 r8 a,4 h,8 ~ |
    h,2 r8 a h4 |
    e2 r8 a,4 h,8 |
    r h a fis a h g4 |
    e1 |
}

\include "drumpitch-init.ly"
drhi = \notes {
    \drhi
    \repeat percent 10 { cymr2 cymr }
    cymr1
}
drlo = \notes {
    \drlo
    s1*11
}
