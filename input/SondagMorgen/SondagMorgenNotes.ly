\include "deutsch.ly"

marks = \lyrics {
  "\large\framebox{\bf{INTRO}}"1*12
  "\large\framebox{\bf{VERS}}"1*7/8*16 ""2 ""1
  "\large\framebox{\bf{OMKVÆD}}"1*26
  "\large\framebox{\bf{OUTRO}}"1
}

ned = \downbow
op = \upbow

#(define meter '(rows (music "noteheads-2" ((kern . -0.1) "flags-stem")) " = 125"))

global = \notes {
    \global
    s8
    \mark #meter
    s2..
    s1*11
    \mark #'(music "scripts-segno")
    \bar "|:"
    s4*54
    \property Score.repeatCommands = #'((volta "1"))
    s1
    \property Score.repeatCommands = #'((volta #f)(volta "2, 3"))
    \bar ":|"
    s1
    \property Score.repeatCommands = #'((volta #f))
    \bar ".|"
    % OMKVÆD
    s1*11
    \property Score.repeatCommands = #'((volta "1"))
    s1*4 s2..
    \mark #'(rows "D. S.  " (music "scripts-segno"))
    %\mark "D. S."
    s8
    \bar "|."
    \property Score.repeatCommands = #'((volta #f)(volta "2"))
    s1*9
    \property Score.repeatCommands = #'((volta #f)(volta "3"))
    \bar ":|"
    s1*1
    \property Score.repeatCommands = #'((volta #f))
    \bar ".|"
    s1*11
    \bar "|."
}

akk = \lyrics {
    \akk
    "Em7"1 "Hm7" "Em7" "Hm7" "Em7" "Hm7"
    "Em7"1 "Hm7" "Em7" "Hm7" "Em7" "Hm7"

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
    "D11"1
    "H+9$\\sharp$"
    
    % OMKVÆD
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$" "A11" "H11"
    "Em7"2  "G$\\Delta$"
    "A11"2 "H11"
    "Em7"  "G$\\Delta$" "Am7"1 "Dsus4" "D"
    "A11"2 "H11"
    "Em7" "G$\\Delta$" "A11" "H11"
    "C$\\Delta$"1  "D6" "Am9" "Hm7" "C$\\Delta$"1  "H9$\\sharp$"
    "A11"2 "H11"

    %OUTRO
    "Em7"2 "G"2 "Hm7"1 "Em7" "Hm7" "Em7" "Hm7"
    "Em7"1 "Hm7" "Em7" "Hm7" "Em7"


}


mel = \notes \relative g' {
    \mel
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
    \time 7/8
    \key g \minor
    [b a g] a4 [g8 f] |
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
    a2 r8 d,8 [g a]  |
    h2

    % OMKVÆD
    r8 e4 d8 ~ |
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

    r2 r8 e'4 d8 ~ |
    d8 h4 e8~e h g a ~|
    a1 ~ |
    a1 ~ |
    a2 r8 d, g a

    r2 r8 e'4 d8 ~ |
    d8 h4 e8~e h g a ~|
    a g a g a g a g |
    h4. e,8~e2 |
    h'4. e,8~e2 |
    h'4. e,8~e2 |
    h'4. d8~d4 c |
    h4. e,8~e2 |
    h'4 h r8 e4 d8 |

    r2 r8 e4 d8 ~ |

    % OUTRO
    \time 2/2
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

kormel = \notes \relative g' {
    \kormel
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
    \time 7/8
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
    d2 R2
    cisis2 R2
    % OMKVÆD
    \key e \minor
    R1*11
    r2 r8 e4 d8~
    d h4 e8~e d h d~
    d1~d1~d2 r2
    R1*2
    r2 r8 e e e
    \repeat unfold 5 { e4. e8~e2 }
    cisis4 cisis r2
    R1
    \time 2/2
    R1*4
    h2.. a8 ~ |
    a d,2.. |
    g1 |
    d2. r4 |
    h'2.. a8 ~ |
    a d,2.. |
    g1 |

}

sopsax = \notes \relative c''' {
    \sopsax
    % INTRO
    \key e \minor
    \time 2/2
    r2 g(-\p fis d )h1~h1
    h2.. a8~ a fis2.. |
    g2( a )a2 d |
    h2.. a8 ~ a fis2.. |
    g2( a )a2 r2
    % VERS
    \context Voice=sax {
    \key g \minor
    \time 7/8
    s1*0_"1 $\\times$ tacet" R1*7/8
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
    a2 r2 |
    cisis4 r8 d8 r dis4 e8
    
    % OMKVÆD
    \key e \minor
    r4 g,8-\mf r r fis r4 |
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
    %volte 1
    a,8 h4 d8 r d4 e8-\sfz |
    r4 g,8-\mf r r fis r8 e8~ |
    e4. g'8->-\f ~g \> g e4 |
    g-> a8 d, ~ d4 \! d |
    d2-\p r |
    %volte 2
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
    %volte 3
    a,8 h4 d8 r d4 e8 |

    }
    %OUTRO
    \key e \minor
    \time 2/2
    r2 g-\p( fis d )h1~h1
    h2.. a8~ a fis2.. |
    g2( a )a2 d |
    h2.. a8 ~ a fis2.. |
    g2 r2
}
altsax = \notes \relative c'' {
    \altsax
    % INTRO
    \key e \minor
    \time 2/2
    r1 r1 r1 r1
    g2-\p~g8 a g fis ~ fis d2..|
    e2( g )fis a |
    g2~g8 a g fis ~ fis d2..|
    e2( g )fis r2

    %VERS
    \context Voice=sax {
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
    e2 r2 |
    a4 r8 d8 r dis4 e8

    % OMKVÆD
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
    %volte 1
    a,8 h4 d8 r d4 e8-\sfz |
    r4 e,8-\mf r r d r8 c |
    ~ c4. d'8->-\f ~d8 \> d c4 |
    d4 e8 d ~ d4 \! h |
    a2-\p r |
    %volte 2
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
    %volte 3
    a,8 h4 d8 r d4 e8 |
    }
    % OUTRO
    \key e \minor
    \time 2/2
    r1 r1 r1 r1
    g,2-\p ~g8 a g fis ~ fis d2..|
    e2( g )fis a |
    g2~g8 a g fis ~ fis d2..|
    e2 r

}
tensax = \notes \relative c'' {
    \tensax
    % INTRO
    \key e \minor
    \time 2/2
    r1 r1 r2 g-\p( fis d
    )e2.. d8 ~ d h2.. |
    h2( d )d d |
    e2.. d8 ~ d h2.. |
    h2( d )d r

    % VERS
    \context Voice=sax {
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
    c2 r2 |
    dis4 r8 d8 r dis4 e8 |

    % OMKVÆD
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
    %volte 1
    a,8 h4 d8 r d4 e8-\sf |
    r4 h8-\mf r r a r g |
    ~ g4. a'8->-\f ~ a \> a g4 |
    a4-> a8 a ~ a4 \! g |
    fis2-\p r|
    %volte 2
    a,8 h4 d8 r d4 e8-\sfz |
    r4 h8-\mf r r a r4 |
    r8 g r4 r8 a r4 |
    s1*0-\mp
    \repeat unfold 2{
	d4.-. d8 \< ~d4 \! r4*1/2 \> \! s4*1/2 |
	e4.-. e8 \< ~e4 \! r4*1/2 \> \! s4*1/2  |
    }
    d4.-. d8 \< ~d4 \! r4*1/2 \> \! s4*1/2 |
    dis4-\ff dis8 d r dis4 e8 | 
    %volte 3
    a,8 h4 d8 r d4 e8 |
    }
    % OUTRO
    \key e \minor
    \time 2/2
    r1 r1 r2 g-\p( fis d
    )e2.. d8 ~ d h2.. |
    h2( d )d d |
    e2.. d8 ~ d h2.. |
    h2 r

}

piar = \notes \relative c' {
    \piar
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

    % VERS
    \time 7/8
    \key g \minor
    \repeat percent 3 {
	[<d,8 b'> g d] [<c a'> f][<c a'> f] |
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
    <c e g a> r8 d g a |
    <dis,4 fis a h cisis> r8 <d8-\f a' d> r <dis4 ais' dis> <e8 h' e> | 
    % OMKVÆD
    \key e \minor
    s1*3
    r8 <g,4 a h d e> <a8 h cis e> r8 <d4 a' d> <e8 h' e> |

    r2 r4 r8 <g, a h d>~|
    <g4. a h d> <a8 h cis e>~<a4. h cis e> <g8 h d e g>|
    s1
    r8 <g4 a h d e> <d'8 a' d> r <dis4 ais' dis> <e8 h' e>
    s1*3
    %volte 1
    r8 <g,4 a h d e> <a8 h cis e> r8 <d4 a' d> <e8 h' e> |
    r2 r4 r8 <g, c e > ~|
    <g1 c e > |
    <d' e g a>|
    <a2 d fis> r8 d g a |
    %volte 2
    r8 <g,4 a h d e> <a8 h cis e> r8 <d4 a' d> <e8 h' e> |
    s1*2
    <g,1-\mf \< h d e g>
    <a d e fis>
    <g c d e g>
    <a d e fis>
    <g1 \! h d e g> |
    <dis'4.-\ff fis a h cisis> <d8 a' d> r8 <dis4 ais' dis> <e8 h' e> |
    %volte 3
    r8 <g,4 a h d e> <a8 h cis e> r8 <d4 a' d> <e8 h' e> |
    % OUTRO
    r2 <d-\mf g h>
    % s1*0-\p
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
U = \sustainUp
D = \sustainDown
pial = \notes {
    \pial
    \context Voice
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
    % VERS
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
    c2 \D a,2 \U \D |
    d1*1/2 \U \D s2 \U |
    h,4 r8 d8 r dis4 e8  |


    % OMKVÆD
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
    % volte 1
    a,, a, e, <h,,8 h,> r <d,,4 d,> e,8 | 
    e,,8 e, \su <g h d' e' g'> \sd g, g,, \su <g a h d' fis'> \sd e, <a,, a,>~
    <a,,1 a,> |
    <d, d> |
    <d,2 d> r |
    % volte 2
    a,,8 a, e, <h,,8 h,> r <d,,4 d,> e,8 | 
    e,,8 e, \su <g h d' e' g'> \sd g, g,, \su <g a h d' fis'> \sd e, a,
    a,, \su <g a h d' e' > \sd e, h, h,, \su <a h cis' e'> \sd d, e, |
    <c,1 c>
    <d, d>
    <a,, a,>
    <h,, h,>
    <c, c>
    <h,,4. h,> <d,8 d> r <dis,4 dis> <d,8 d> |

    a,,8 a, e, <h,,8 h,> r <d,,4 d,> e,8 | 
    \clef "F"
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
guitar = \notes \transpose c' {
    \guitar
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
    % VERS
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


	<d,2-2 g-3 h-4 e-5 a-6> r2 |
    }
    r4 e8-2 < a-3 d'-4> e-2 < ais-3 dis'-4> e-2 < h-3 e'-4> |
    % OMKVÆD
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
    % volte1
    e-2 e-2 < a-3 d'-4> e-2 e-2 < a-3 d'-4> e-2 < h-3 e'-4> |
    e8-2 < d'-4 g'-5> e-2 e-2 < e'-4 a'-5> e-2 e-2 <g8-3 c'-4 e'-5 a'-6> ~ |
    <g1-3 c'-4 e'-5 a'-6>
    <a1-3 d'-4 e'-5 a'-6> ~
    <a2-3 d'-4 fis'-5 a'-6> r |
    %volte2
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
    %volte3
    e8-2 e-2 < a-3 d'-4> e-2 e-2 < a-3 d'-4> e-2 < h-3 e'-4> |
    % OUTRO
    \time 2/2
    \key e \minor
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

cello = \notes \relative c {
    \cello
    \time 2/2
    \key e \minor
    e1 d2( fis g h d fis )e1 fis2( d
    )e1 d2( h g e fis d )e1 h
    \time 7/8
    \key g \minor
    R1*7/8*7
    \time 9/8
    R1*9/8
    \time 7/8
    \key g \major
    R1*7/8*6
    \time 4/4
    R1 R1 R1
    \key e \minor
    \property Score.skipBars = ##t
    R1*11
    R1*5
    R1*9
    R1
    \time 2/2
    e1 d2( fis g h d fis )e1 fis2( d
    )e1 d2( h g e fis d )e1
}

bas = \notes {
    \bas
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
    % VERS
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
    d1 |
    h,4 r8 d8 r dis4 e8  |

    %OMKVÆD
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

    a, a, r h, r d4 e8 |
    e,8 e, r8 g, g, g, r a,~ |
    a,2. ~ a,4 \glissando |
    d1 |
    r2 d4 d |

    a,8 a, r h, r d4 e8 |
    e,8 e, r8 g, g, g, r a, |
    a, a, r h, h, h, r4 |
    c4. c'8 r c'4.
    d4. d'8 r d'4.
    a,4. a8 r a4.
    h,4. h8 r h4.
    c4. c'8 r c'4.
    h,4 h,8 d8 r dis4 e8 |

    a,8 a, r h, r d4 e8~ |
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
    \time 2/2
    \repeat percent 12 { cymr2 cymr } |
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
    cymc4 tommh8 toml sn hho sn sn |
    cymc4. cymc8 ~ cymc cymc4 cymc8 |
    
    % OMKVÆD
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
 
    %volte 1
    hh hh hh hho r cymc4 cymc8
    hh8 hh hh hh hh hh hh cymc ~
    cymc4. tommh8 r tommh toml4  |
    cymr4. cymr8~cymr4 cymr4 |
    cymr2 hhc4 hho |

    %volte 2
    hh8 hh hh hho r cymc4 cymc8 |
    hh8 hh hh hh hh hh hh hh |
    hh8 hh hh hh hh hh hh hho |
    \repeat percent 5 { hh8 hh hh hho hh hho hh hh16 hh }
    hh8 hh hho cymc8~cymc cymc4 cymc8 |

    %volte 3 
    hh8 hh hh hho r cymc4 cymc8 |

    \repeat percent 10 { cymr2 cymr }
    cymr1

}
drlo = \notes {
    \drlo
    \time 2/2
    s1*12
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
    bd4. bd8 bd4 bd4
    bd4 sn8 bd r bd4 bd8 |
    

    %OMKVÆD
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

    %volte 1
    bd8 sn bd bd hhp bd4 bd8 |
    bd4 sn8 bd bd4 sn8 bd |
    bd4. bd8 r bd bd4 |
    bd4. bd8 r2 |
    r2 bd4 <
	bd8 sn
    > sn  |

    %volte 2
    bd8 sn bd bd hhp bd4 bd8 |
    bd4 sn8 bd bd4 sn8 bd |
    bd4 sn8 bd bd4 < 
	bd sn
    > |
    \repeat percent 5 { bd4 bd8 bd bd4 bd } |
    bd8 sn bd bd r bd4 bd8 |

    %volte 3
    bd8 sn bd bd hhp bd4 bd8 |
    

    
}
