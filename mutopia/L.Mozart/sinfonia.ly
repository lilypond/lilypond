	%{
Dit is de fout:

GNU LilyPond 1.1.19.
Parsing...[/home/lily/usr/share/lilypond/ly/init.ly[/home/lily/usr/share/lilypond/ly/declarations.ly[/home/lily/usr/share/lilypond/ly/dynamic.ly][/home/lily/usr/share/lilypond/ly/nederlands.ly][/home/lily/usr/share/lilypond/ly/chord-modifiers.ly][/home/lily/usr/share/lilypond/ly/script.ly][/home/lily/usr/share/lilypond/ly/paper20.ly[/home/lily/usr/share/lilypond/ly/table20.ly][/home/lily/usr/share/lilypond/ly/table13.ly][/home/lily/usr/share/lilypond/ly/table16.ly][/home/lily/usr/share/lilypond/ly/params.ly[/home/lily/usr/share/lilypond/ly/a4.ly][/home/lily/usr/share/lilypond/ly/paper.ly][/home/lily/usr/share/lilypond/ly/engraver.ly]]][/home/lily/usr/share/lilypond/ly/midi.ly[/home/lily/usr/share/lilypond/ly/performer.ly]][/home/lily/usr/share/lilypond/ly/property.ly][/home/lily/usr/share/lilypond/scm/lily.scm]][Sinfonia.ly]]
Interpreting music...[8][16][24][32][40]
Sinfonia.ly:239:24: warning: barcheck failed by: 1/4:
	\times 2/3 {[e g fis] |
                                [e fis g]} | fis4 r |

Sinfonia.ly:87:34: warning: barcheck failed by: 1/4:
	\times 2/3 {[g'8 b a] [g fis e] |
                                          [a d, cis] [d cis d] |

Sinfonia.ly:240:25: warning: barcheck failed by: 1/4:
	\times 2/3 {[e8 g fis] |
                                 [e fis g]} | fis4 r |

Sinfonia.ly:88:21: warning: barcheck failed by: 1/4:
	[g, b a] [g fis e] |
                             [a d, cis] [d cis d]} |
[48][56][64][72][80][88][96][104][112][120][123]
time: 24.16 seconds
Preprocessing elements... lilypond: score-element.cc:134: class
Paper_def * Score_element::paper() const: Assertion `pscore_l_' failed.

%}

\header{
filename="Sinfonia.ly";
title="Sinfonia";
subtitle="G major";
composer="Leopold Mozart";
arranger="(1719-1787)";
copyright="public domain";
enteredby="Maarten Storm";
}


global = \notes{
	\key G;
	\time 2/4;
	s2*4 \break 
	s2*5 \break
	s2*5 \break
	s2*5 \break
	s2*5 \break
	s2*5 \break
	s2*5 \break
	s2*5 \break
	s2*5 \break
	s2*5 \break
	s2*6 \bar ":|"; \break
	\bar "|:"; s2*6 \break
	s2*5 \break
	s2*5 \break
	s2*5 \break
	s2*5 \break
	s2*5 \break
        s2*5 \break
        s2*5 \break
	s2*5 \break
        s2*5 \break
        s2*5 \break
	s2*4 \break
	s2*5 \bar ":|";
%fine
}

tempi = \notes{
	\property Voice.textstyle = "large"
        s8^"Allegro"
	
}

dynamics = \notes{
%	\type Voice=i
}


violinoi = \notes \relative c'{
	\type Voice=i
	\clef "violin";
	[<g8 d' b' g'> g'' g g] | [g16 a b a] [g a b a] |
	[<g,,8 d' b' g'> g'' g g] | [g16 a b a] [g a b a] |
%5	
	[<g,,8 d' b' g'> g'' g g] | [g16 d c b] [a g fis g] |
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
%11	
	[fis g a b] [c a g fis] | [g a b c] [d b a g] |
	[a b c d] [e c b a] | [b c d c] [d b a g] |
%15	
	[fis g a g] [fis e d c] | [b a g8] r4 |
	[<g8 d' b' g'> g'' g g] | \times 2/3 {[g8 d c] [b a g] |
	[g'8\p d c] [b a g] |  
%20	
	[f'\f d c] [b a g]} | 
	[<c,8 e c' e> e' e e] | <c,2 e c' e> |
        [<a8 e' cis' a'> a'' a a] |
	\times 2/3 {[a8 e d] [cis b a] | [a'\p e d] [cis b a] |
%26	
	[g'\f e d] [cis b a]} | [<d,8 a' fis'> fis' fis fis] |
	<d,2 a' fis'> | \times 2/3 {[b'8 c d] [e fis g] |
	[g fis g] [b, a g] | [a b cis] [d e fis] |
%32
	[fis e fis] [a, g fis] | [g a b] [cis d e] |
	[e d e] [g, fis e]}
%35
	[fis16 d e d] [e d e d] | [g d e d] [e d e d] |
	[a' d, e d] [e d e d] | [b' d, e d] [e d e d] |
	[cis' a b a] [b a b a] | [d a b a] [b a b a] |
	[e' a, b a] [a b a b] | [fis' a, b a] [a b a b] |
%43
	\times 2/3 {[g'8 b a] [g fis e] | [a d, cis] [d cis d] |
	[g, b a] [g fis e] | [a d, cis] [d cis d]} |
%47 
	[b''16 g fis e] [a fis e d] | [g e d cis] [fis d cis b] | gis,2 |
%50
	a4 g'! | [fis16 fis e d] e4 | d d' | r8 r16 gis,16 a4 |
	r8 [fis g! a] | d,2 
	[d'8\p e f a,] | [gis a] r4 | 
	\times 2/3 {[f8\f e d] [c b a]} | [gis a] r4 |
%60
	[c'8\p d e g,?] | [fis g] r4 |
	\times 2/3 {[d8\f a' c] [b a b]} | [b a] r4 |
%64 : reprise
	[<g,8 d' b' g'> g'' g g] | [g16 a b a] [g a b a] |
        [<g,,8 d' b' g'> g'' g g] | [g16 a b a] [g a b a] |
        [<g,,8 d' b' g'> g'' g g] | [g16 d c b] [a g fis g] |
%70        
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
        [fis g a g] [fis e d c] | [b a g8] r4 |
        [<g8 d' b' g'> g'' g g] | \times 2/3 {[g8 d c] [b a g] |
	[f\f d c] [b a g]} | [e' c] r4 |
%78	
	[<a8 e' cis' a'> a'' a a] |
        \times 2/3 {[a8 e d] [cis b a] | 
        [g e d] [cis b a]} | [fis' d] r4 |	
	\times 2/3 {[e'8 fis g] [a b c] | [c b c] [e, d c] |
	[d e fis] [g a b] | [b a b] [d, c b] | [c d e] [fis g a] |
	[a g a] [c, b a]} |
%88
	[b16 g a g] [a g a g] | [c g a g] [a g a g] | [d' g, a g] [a g a g] |
	[e' g, a g] [a g a g] | [fis' d e d] [e d e d] | [g d e d] [e d e d] |
	[a' d, e d] [e d e d] | [b' d, e d] [e d e d] | 
%96
	\times 2/3 {[c'8 e, d] [c b a] | [d g, fis] [g fis g] | 
	[d c' c] [c b a] | [d g, fis] [g fis g]} |
%100
	[e'16 c b a] [d b a g] | [c a g fis] [b g fis e] | cis2 | d4 c'! |
	[b16 b a g] a4 | g g' | 
%106
	r8 r16 cis,, d4 | r8 [b c? d] | [g,16 g'' g g] g4:16 | 
	[fis16 g a g] [fis e d c] | [b g' g g] g4:16 | 
	[fis16 g a g] [fis e d c] | [b g' g, a] a4 |
	[b16 g' g, a] a4 | [b16 g' g, a] a4 | [g8 d] g,4 
%fine  

}

violinoii = \notes \relative c'{
	\type Voice=i
	\clef "violin";
	[<g8 d' b' g'> g'' g g] | [b,16 c d c] [b c d c] |
	[<g,8 d' b' g'> g'' g g] | [b,16 c d c] [b c d c] |
%5
	[<g,8 d' b' g'> g'' g g] | [g16 d c b] [a g fis g] |
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
%11	
	[fis g a b] [c a g fis] | [g a b c] [d b a g] |
        [a b c d] [e c b a] | [b c d c] [d b a g] |
%15
        [fis g a g] [fis e d c] | [b a g8] r4 |
	[<g8 d' b' g'> g'' g g] | \times 2/3 {[g8 d c] [b a g] |
	[g'8\p d c] [b a g] | [f'\f d c] [b a g]} |
%21
	[<c,8 e c' e> e' e e] | <c,2 e c' e> |
	[<a8 e' cis' a'> a'' a a] |
	\times 2/3 {[a8 e d] [cis b a] | [a'\p e d] [cis b a] |
%26   
        [g'\f e d] [cis b a]} | [<d, a' fis'> fis' fis fis] |
        <d,2 a' fis'> | \times 2/3 {[b'8 c d] [e fis g] |
        [g fis g] [b, a g] | [a b cis] [d e fis] |
%32
        [fis e fis] [a, g fis] | [g a b] [cis d e] |
        [e d e] [g, fis e]}
%35
        [fis16 d e d] [e d e d] | [g d e d] [e d e d] |
        [a' d, e d] [e d e d] | [b' d, e d] [e d e d] |
        [cis' a b a] [b a b a] | [d a b a] [b a b a] |
        [e' a, b a] [a b a b] | [fis' a, b a] [a b a b] |
%43
        \times 2/3 {[g'8 b a] [g fis e] | [a d, cis] [d cis d] |
        [g, b a] [g fis e] | [a d, cis] [d cis d]} |
%47
        [b''16 g fis e] [a fis e d] | [g e d cis] [fis d cis b] | gis,2 |
%50
        a4 g'! | [fis16 fis e d] e4 | d d' | r8 r16 g,16 a4 |
        r8 [fis g! a] | d,2
	f4\p r8 f | [e-. ~ e-.] r4 |
	\times 2/3 {[f8\f e d] [c b a]} | [gis a] r4 |
%60
	e'4\p r8 e | [d-. ~ d-.] r4 |
	\times 2/3 {[d8\f a' c] [b a b]} | [b a] r4 |
%64 : reprise
	[<g,8 d' b' g'> g'' g g] | [b,16 c d c] [b c d c] |
        [<g,8 d' b' g'> g'' g g] | [b,16 c d c] [b c d c] |
        [<g,8 d' b' g'> g'' g g] | [g16 d c b] [a g fis g] |
%70        
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
        [fis g a g] [fis e d c] | [b a g8] r4 |
        [<g8 d' b' g'> g'' g g] | \times 2/3 {[g8 d c] [b a g] |
	[f\f d c] [b a g]} | [e' c] r4 |
%78
        [<a8 e' cis' a'> a'' a a] |
        \times 2/3 {[a8 e d] [cis b a] | 
        [g e d] [cis b a]} | [fis' d] r4 |
	\times 2/3 {[e'8 fis g] [a b c] | [c b c] [e, d c] |
        [d e fis] [g a b] | [b a b] [d, c b] | [c d e] [fis g a] |
        [a g a] [c, b a]} |
%88
        [b16 g a g] [a g a g] | [c g a g] [a g a g] | [d' g, a g] [a g a g] |
        [e' g, a g] [a g a g] | [fis' d e d] [e d e d] | [g d e d] [e d e d] |
        [a' d, e d] [e d e d] | [b' d, e d] [e d e d] |
%96
        \times 2/3 {[c'8 e, d] [c b a] | [d g, fis] [g fis g] |
        [d c' c] [c b a] | [d g, fis] [g fis g]} |
%100
        [e'16 c b a] [d b a g] | [c a g fis] [b g fis e] | cis2 | d4 c'! |
        [b16 b a g] a4 | g g' |
%106
        r8 r16 cis,, d4 | r8 [b c? d] | [g,16 g'' g g] g4:16 |
        [fis16 g a g] [fis e d c] | [b g' g g] g4:16 |
        [fis16 g a g] [fis e d c] | [b g' g, a] a4 |
        [b16 g' g, a] a4 | [b16 g' g, a] a4 | [g8 d] g,4
%fine

}

viola = \notes \relative c'{
	\type Voice=i
	\clef "alto";
	[b16 c d c] [b c d c] | [b8 b b b] | 
	[b16 c d c] [b c d c] | [b8 b b b] |
%5
	[b16 c d c] [b c d c] | b4 r |
	r8 [e g a] | [d, b b d] | [g, e' g a] |
%10
	[d, b d e] | [a, fis'16 g] [a fis e d] |
	[d8 d d d] | r [a' a a] | [d, b d e] |
	[a, d d a] | [g16 g' fis e] [d c b a] |
	[g b d c] [b c d c] | b4 r | d d | d d |
%21
	[e16 f? g f] [e f g f] | [g8 e e d] |
	[cis16 d e d] [cis d e d] | cis4 r | 
%25
	e\p e | e\f e | [fis16 g a g] [fis g a g] | 
	[fis8 fis a a] | d4 b | b g' | cis,? a |
	a fis' b, g' | a a, |
%35
	[a8 fis' fis fis] | r [g g g] | r [a a a] |
	r [b, b b] | r [cis cis cis] | r [d d d] | r [e e e] |
	r [fis fis fis] |
%43
	\times 2/3 {[e g fis] | [e fis g]} | fis4 r |
	\times 2/3 {[e8 g fis] | [e fis g]} | fis4 r |
%47
	[d8 g d fis] | r [e a, d] | r b [b cis16 d] |
	[e8 d e cis] | [d d d cis] | d4 d' | r8 r16 gis, a4 |
	r8 [fis8 g! a] | d,2  
	[d8\p d d d] | [d c!] r4 |
	\times 2/3 {[f8\f e d] [c b a]} | [gis a] r4 |
%60
	[c8\p c c c] | [c b] r4 | [a8 fis e g] | [g fis] r4 |
%64 : reprise
	[b,16 c d c] [b c d c] | [b8 b b b] |
        [b16 c d c] [b c d c] | [b8 b b b] |
%68
        [b16 c d c] [b c d c] | b4 r |
	r8 [e g a] | [d b] r e | [a, a a a] |
%73
	[g16 b c d] [e fis g a] | [b c d c] [b c d c] | b4 r |
	d, d | g, r | [cis16 d e d] [cis d e d] | cis 4 r | 
%80
	e4 e | [a,8 fis'16 g] [fis8 d] |
	g,4 e' | e c' | fis, d | d b' | e, c | c d |
%88
	[d8 b b b] | r [c c c] | r [d d d] | r [e e e] | r [fis fis fis] |
	r [g g g] | r [a a a] | r [b b b] | 
%96
	\times 2/3 {[a c b] [a b c]} | b4 r | \times 2/3 {[a,8 a g] [a b c]} |
	b4 r | [g8 c g b] | r [a' d, g] | r e [e fis16 g] | [a8 g a fis] |
%104
	g8 g4 fis8 | g4 g | r8 r16 cis, d4 | r8 [b c! d] | [g, b d e] | 
	[a, a a' a] | [d, b d e] | [a, a a' a] | [d, d e d] | [d d e d] | 
	[d g g fis] | [g d] g,4
%fine 
 
}

bassocontinuo = \notes \relative c'{
	\type Voice=i
	\clef "bass";
	[g16 a b a] [g a b a] | [g8 g, g g'] |
	[g16 a b a] [g a b a] | [g8 g, g g'] | 
%5
	[g16 a b a] [g a b a] | [g8 g,] r4 |
	[c'8 c,] [e_"6" fis_"6"_"5" | [g g g b,_"6"] |
	[c c] [e_"6" fis_"6"_"5" | [g g b_"6" c] |
%11
	[d d d c_"2"] | [b_"6" b b b] | [fis_"6"_"5" fis fis fis] |
	[g g b_"6" c] | [d fis,_"6"_"5" fis fis] | [g16 g fis e] [d c b a] |
%17
	[g g' b a] [g a b a] | g4 r | b, b | b b |
%21
	[c16 d e d] [c d e d] | [c8 e' c a] | [a16 b cis b] [a b cis b] 
	a4 r | cis, cis | cis cis |
%27
	[d16 e fis e] [d e fis e] | [d8 d fis fis] | g4 g | 
	e e | fis fis | d d | e e | cis cis |
%35
	d2:8 | e2:8 | fis2:8 | g2:8 | a2:8 | b2:8 | [cis8 cis,? cis cis] |
	d2:8 | cis4 cis | d r | cis cis | d r |
%47
	g8 r fis r | e r d r | r [e e d] | [cis b cis a] | [d fis g a] |
	d,4 d' | r8 r16 gis, a4 | r8 [fis g! a] | d,2 
	r2 | r |
	\times 2/3 {[f8\f e d] [c b a]} | [gis a] r4 |
%60
	r2 | r |
	[f'8 d g g,] | [d' d'] [d16 c b a] |
%64 : reprise
	[g16 a b a] [g a b a] | [g8 g, g g'] |
        [g16 a b a] [g a b a] | [g8 g, g g'] |
%68
        [g16 a b a] [g a b a] | g4 r |
	[c8 c,] [e fis] | [g g b c] | [e fis fis fis] |
%73
	[g16 g, a b] [c d e fis] | [g a b a] [g a b a] | g4 r |
	b, b | [c8 c'16 d] [e d cis b] | [a b cis b] [a b cis b] |
%79
	a4 r | cis,4 cis | [d8 d'16 e] [d8 b] |	
	c?4 c | a a | b b | g g | a a | fis fis | g2:8 | a2:8 |
%90
	[b8 b, b b] | c2:8 | d2:8 | e2:8 | fis2:8 | g2:8 | 
	fis4 fis | g r | fis fis | g r | c8 r b r | a r g r |
	r [a a g] | [fis e fis d] | [g, b c d] | g,4 g' |
%106
	r8 r16 cis, d4 | r8 [b c! d] | [g, g' b c] | [d fis, fis fis] |
	[g g, b c] | [d fis fis fis] | [g b, c d] | [g, b c d] | 
	[g, b c d] | [g d] g,4
%fine
}


\score{
        \type StaffGroup <
                \type Staff = i < \tempi \global \dynamics \violinoi >
                \type Staff = ii < \global \dynamics \violinoii >
                \type Staff = iii < \global \dynamics \viola >
                \type Staff = iv < \global \dynamics \bassocontinuo >
        >
        \paper{
          %      \translator { \OrchestralScoreContext }
        }
        \midi{ \tempo 4 = 100; }
}




