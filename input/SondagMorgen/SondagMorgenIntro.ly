\include "deutsch.ly"

marks = \lyrics {
  "\large\framebox{\bf{INTRO}}"1*12
}

ned = \downbow
op = \upbow

#(define meter '(rows (music "noteheads-2" ((kern . -0.1) "flags-stem")) " = 128"))

global = \notes {
    s8
    \mark #meter
    s2..
    s1*11
}

akk = \lyrics {
    "Em7"1 "Hm7" "Em7" "Hm7" "Em7" "Hm7"
    "Em7"1 "Hm7" "Em7" "Hm7" "Em7" "Hm7"
}

mel = \notes \relative g' {
    \time 2/2
    \key e \minor
    R1*4
    g2 ~ g8 a g fis ~ |
    fis h,2.. |
    e1 |
    fis2. r4 |
    g2 ~ g8 a g fis ~ |
    fis h,2.. |
    e1 |
    fis2

    r8 d8 [g a]  |
}

kormel = \notes \relative g' {
    \time 2/2
    \key e \minor
    R1*4
    h2.. a8 ~ |
    a d,2.. |
    g1 |
    d2. r4 |
    h'2.. a8 ~ |
    a d,2.. |
    g1 |
    d2 R2 |
}

sopsax = \notes \relative c''' {
    % INTRO
    \key e \minor
    \time 2/2
    r2 g(-\p fis d )h1~h1
    h2.. a8~ a fis2.. |
    g2( a )a2 d |
    h2.. a8 ~ a fis2.. |
    g2( a )a2 r2
}
altsax = \notes \relative c'' {
    % INTRO
    \key e \minor
    \time 2/2
    r1 r1 r1 r1
    g2-\p~g8 a g fis ~ fis d2..|
    e2( g )fis a |
    g2~g8 a g fis ~ fis d2..|
    e2( g )fis r2
}
tensax = \notes \relative c'' {
    % INTRO
    \key e \minor
    \time 2/2
    r1 r1 r2 g-\p( fis d
    )e2.. d8 ~ d h2.. |
    h2( d )d d |
    e2.. d8 ~ d h2.. |
    h2( d )d r
}

piar = \notes \relative c' {
    \time 2/2
    \key e \minor
    % a1*0-\p
    \repeat unfold 2 {
	<d1-\arpeggio e g a d> 
	R1
	fis8( g d' fis, g d' fis, )g |
	<a1-\arpeggio h d e a> 
	R1
    }
    \alternative {
	{ fis8( g d' fis, g d' fis, )g | }
	{ fis8( g d' fis, g d fis )a | }
    }

}
U = \sustainUp
D = \sustainDown
pial = \notes {
    \clef "F"
    \property Staff.SustainPedal \override #'pedal-type = #'mixed
    \relative c {
	\time 2/2
	\key e \minor
	\repeat unfold 2 {
	    <e1 \D h'>
	    <h \U \D  fis' d'>
	    <e1 \U \D h'>
	    <h \U \D fis' d'>
	    <e1 \U \D h' g'>
	    <h1*7/8 \U \D fis' d'>
	    s8 \U
	}
    }
}
guitar = \notes \transpose c' {
    \time 2/2
    \key e \minor
    \property Staff.Arpeggio \override #'arpeggio-direction = #1
	g'8-5_"FLANGE" fis'4-5 d'8-4 r8 h4.-3 |
	a8-3( d'-4 a'-5 a-3 d'-4 a'-5 a-3 )d'-4 |
	<e1-\arpeggio-2 a-3 d'-4 g'-5 h'-6>
	g'8-5 fis'4-5 d'8-4 r8 h4.-3 |
	a8-3( d'-4 a'-5 a-3 d'-4 a'-5 a-3 )d'-4 |
	<e1-\arpeggio-2 a-3 d'-4 fis'-5 h'-6>
	g'8-5 fis'4-5 d'8-4 r8 h4.-3 |
	a8-3( d'-4 a'-5 a-3 d'-4 a'-5 a-3 )d'-4 |
	<e1-\arpeggio-2 a-3 d'-4 g'-5 h'-6>
	g'8-5 fis'4-5 d'8-4 r8 h4.-3 |
	a8-3( d'-4 a'-5 a-3 d'-4 a'-5 a-3 )d'-4 |
	<e1-\arpeggio-2 a-3 d'-4 fis'-5 h'-6>

}

cello = \notes \relative c {
    \time 2/2
    \key e \minor
    e1 d2( fis g h d fis )e1 fis2( d
    )e1 d2( h g e fis d )e1 h
}

bas = \notes {
    \time 2/2
    \key e \minor
    \repeat unfold 2 {
	e2 r8 a,4 h,8 ~ |
	h,2 r8 a h4 |
	e2 r8 a,4 h,8 |
	r h a fis a h g4 |
	e2 r8 d4 h,8 ~ |
	h,2 r2
    }
}

\include "drumpitch-init.ly"
drhi = \notes {
    \time 2/2
    \repeat percent 12 { cymr2 cymr } |

}
drlo = \notes {
    \time 2/2
    s1*12
}
