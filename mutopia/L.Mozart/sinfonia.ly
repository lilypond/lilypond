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
	s2*4 \break
	s2*5 \bar ":|";

%fine
}

tempi = \notes{
	\property Voice.textStyle = "large"
        s8^"Allegro"
	
}

dynamics = \notes{
	\context Voice=i
	s16\f s8. s4 | s2*17
	s8\p s4. | s8\f s4. | s2*4
	s8\p s4. | s8\f s4. | s2*29
	s8\p s4. | s2 |
	s8\f s4. | s2 | s8\p s4. | s2 | s8\f s4. | s2*47
	s16 s16\p s4. | s2 |
	s16 s16\f s4. |
	s16 s16\p s4. |
	s16 s16\f s4. |	
}


violinoi = \notes \relative c'{
	\context Voice=i
	\clef "violin";
	\stemup
	[<g8 d' b' g'> g'' g g] \stemboth | [g16 a b a] [g a b a] |
	\stemup [<g,,8 d' b' g'> g'' g g] \stemboth | [g16 a b a] [g a b a] |
%5	
	\stemup [<g,,8 d' b' g'> g'' g g] \stemboth | [g16 d c b] [a g fis g] |
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
%11	
	[fis g a b] [c a g fis] | [g a b c] [d b a g] |
	[a b c d] [e c b a] | [b c d c] [d b a g] |
%15	
	[fis g a g] [fis e d c] | [b a g8] r4 |
	\stemup [<g8 d' b' g'> g'' g g] \stemboth | 
	\times 2/3 {[g8 d c]} \times 2/3 {[b a g]} |
	\times 2/3 {[g'8 d c]} \times 2/3 {[b a g]} |  
%20	
	\times 2/3 {[f' d c]} \times 2/3 {[b a g]} | 
	\stemup [<c,8 e c' e> e' e e] | <c,2 e c' e> |
        [<a8 e' cis' a'> a'' a a] \stemboth |
	\times 2/3 {[a8 e d]} \times 2/3 {[cis b a]} | 
	\times 2/3 {[a' e d]} \times 2/3 {[cis b a]} |
%26	
	\times 2/3 {[g' e d] [cis b a]} | 
	\stemup [<d,8 a' fis'> fis' fis fis] \stemboth|
	<d,2 a' fis'> | \times 2/3 {[b'8 c d]} \times 2/3 {[e fis g]} |
	\times 2/3 {[g fis g]} \times 2/3 {[b, a g]} | 
	\times 2/3 {[a b cis]} \times 2/3 {[d e fis]} |
%32
	\times 2/3 {[fis e fis]} \times 2/3 {[a, g fis]} | 
	\times 2/3 {[g a b]} \times 2/3 {[cis d e]} |
	\times 2/3 {[e d e]} \times 2/3 {[g, fis e]} |
%35
	[fis16 d e d] [e d e d] | [g d e d] [e d e d] |
	[a' d, e d] [e d e d] | [b' d, e d] [e d e d] |
	[cis' a b a] [b a b a] | [d a b a] [b a b a] |
	[e' a, b a] [a b a b] | [fis' a, b a] [a b a b] |
%43
	\times 2/3 {[g'8 b a]} \times 2/3 {[g fis e]} | 
	\times 2/3 {[a d, cis]} \times 2/3 {[d cis d]} |
	\times 2/3 {[g, b a]} \times 2/3 {[g fis e]} | 
	\times 2/3 {[a d, cis]} \times 2/3 {[d cis d]} |
%47 
	[b''16 g fis e] [a fis e d] | [g e d cis] [fis d cis b] | gis,2 |
%50
	a4 g'! | [fis16 fis e d] e4 | d d' | r8 r16 gis,16 a4 |
	r8 [fis g! a] | d,2 
	[d'8 e f a,] | [gis a] r4 | 
	\times 2/3 {[f8 e d]} \times 2/3 {[c b a]} | [gis a] r4 |
%60
	[c'8 d e g,?] | [fis g] r4 |
	\times 2/3 {[d8 a' c]} \times 2/3 {[b a b]} | [b a] r4 |
%64 : reprise
	\stemup [<g,8 d' b' g'> g'' g g] | \stemdown [g16 a b a] [g a b a] |
        \stemup [<g,,8 d' b' g'> g'' g g] | \stemdown [g16 a b a] [g a b a] |
        \stemup [<g,,8 d' b' g'> g'' g g] | \stemboth [g16 d c b] [a g fis g] |
%70        
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
        [fis g a g] [fis e d c] | [b a g8] r4 |
        \stemup [<g8 d' b' g'> g'' g g] \stemboth | 
	\times 2/3 {[g8 d c]} \times 2/3 {[b a g]} |
	\times 2/3 {[f d c]} \times 2/3 {[b a g]} | [e' c] r4 |
%78	
	\stemup [<a8 e' cis' a'> a'' a a] \stemboth |
        \times 2/3 {[a8 e d]} \times 2/3 {[cis b a]} | 
        \times 2/3 {[g e d]} \times 2/3 {[cis b a]} | [fis' d] r4 |	
	\times 2/3 {[e'8 fis g]} \times 2/3 {[a b c]} | 
	\times 2/3 {[c b c]} \times 2/3 {[e, d c]} |
	\times 2/3 {[d e fis]} \times 2/3 {[g a b]} | 
	\times 2/3 {[b a b]} \times 2/3 {[d, c b]} | 
	\times 2/3 {[c d e]} \times 2/3 {[fis g a]} |
	\times 2/3 {[a g a]} \times 2/3 {[c, b a]} |
%88
	[b16 g a g] [a g a g] | [c g a g] [a g a g] | [d' g, a g] [a g a g] |
	[e' g, a g] [a g a g] | [fis' d e d] [e d e d] | [g d e d] [e d e d] |
	[a' d, e d] [e d e d] | [b' d, e d] [e d e d] | 
%96
	\times 2/3 {[c'8 e, d]} \times 2/3 {[c b a]} | 
	\times 2/3 {[d g, fis]} \times 2/3 {[g fis g]} | 
	\stemup \times 2/3 {[d c' c]} \times 2/3 {[c b a]} \stemboth | 
	\times 2/3 {[d g, fis]} \times 2/3 {[g fis g]} |
%100
	[e'16 c b a] [d b a g] | [c a g fis] [b g fis e] | cis2 | d4 c'! |
	[b16 b a g] a4^\trill | g g' | 
%106
	r8 r16 cis,, d4 | r8 [b c? d] | [g,16 g'' g g] g4:16 | 
	[fis16 g a g] [fis e d c] | [b g' g g] g4:16 | 
	[fis16 g a g] [fis e d c] | [b g' g, a] a4^\trill |
	[b16 g' g, a] a4^\trill | [b16 g' g, a] a4^\trill | [g8 d] g,4 
%fine  

}

violinoii = \notes \relative c'{
	\context Voice=i
	\clef "violin";
	\stemup [<g8 d' b' g'> g'' g g] \stemboth | [b,16 c d c] [b c d c] |
	\stemup [<g,8 d' b' g'> g'' g g] \stemboth | [b,16 c d c] [b c d c] |
%5
	\stemup [<g,8 d' b' g'> g'' g g] \stemboth | [g16 d c b] [a g fis g] |
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
%11	
	[fis g a b] [c a g fis] | [g a b c] [d b a g] |
        [a b c d] [e c b a] | [b c d c] [d b a g] |
%15
        [fis g a g] [fis e d c] | [b a g8] r4 |
	\stemup [<g8 d' b' g'> g'' g g] \stemboth | 
	\times 2/3 {[g8 d c]} \times 2/3 {[b a g]} |
	\times 2/3 {[g'8 d c]} \times 2/3 {[b a g]} | 
	\times 2/3 {[f' d c]} \times 2/3 {[b a g]} |
%21
	\stemup [<c,8 e c' e> e' e e] | <c,2 e c' e> |
	[<a8 e' cis' a'> a'' a a] \stemboth |
	\times 2/3 {[a8 e d]} \times 2/3 {[cis b a]} | 
	\times 2/3 {[a' e d]} \times 2/3 {[cis b a]} |
%26   
        \times 2/3 {[g' e d]} \times 2/3 {[cis b a]} | 
	\stemup [<d, a' fis'> fis' fis fis] \stemboth |
        <d,2 a' fis'> | \times 2/3 {[b'8 c d]} \times 2/3 {[e fis g]} |
        \times 2/3 {[g fis g]} \times 2/3 {[b, a g]} | 
	\times 2/3 {[a b cis]} \times 2/3 {[d e fis]} |
%32
        \times 2/3 {[fis e fis]} \times 2/3 {[a, g fis]} | 
	\times 2/3 {[g a b]} \times 2/3 {[cis d e]} |
        \times 2/3 {[e d e]} \times 2/3 {[g, fis e]}
%35
        [fis16 d e d] [e d e d] | [g d e d] [e d e d] |
        [a' d, e d] [e d e d] | [b' d, e d] [e d e d] |
        [cis' a b a] [b a b a] | [d a b a] [b a b a] |
        [e' a, b a] [a b a b] | [fis' a, b a] [a b a b] |
%43
        \times 2/3 {[g'8 b a]} \times 2/3 {[g fis e]} | 
	\times 2/3 {[a d, cis]} \times 2/3 {[d cis d]} |
        \times 2/3 {[g, b a]} \times 2/3 {[g fis e]} | 
	\times 2/3 {[a d, cis]} \times 2/3 {[d cis d]} |
%47
        [b''16 g fis e] [a fis e d] | [g e d cis] [fis d cis b] | gis,2 |
%50
        a4 g'! | [fis16 fis e d] e4 | d d' | r8 r16 g,16 a4 |
        r8 [fis g! a] | d,2
	f4 r8 f | [e-. ~ e-.] r4 |
	\times 2/3 {[f8 e d]} \times 2/3 {[c b a]} | [gis a] r4 |
%60
	e'4 r8 e | [d-. ~ d-.] r4 |
	\times 2/3 {[d8 a' c]} \times 2/3 {[b a b]} | [b a] r4 |
%64 : reprise
	\stemup [<g,8 d' b' g'> g'' g g] \stemboth | [b,16 c d c] [b c d c] |
        \stemup [<g,8 d' b' g'> g'' g g] \stemboth | [b,16 c d c] [b c d c] |
        \stemup [<g,8 d' b' g'> g'' g g] \stemboth | [g16 d c b] [a g fis g] |
%70        
	[e' fis g fis] [g e d c] | [b c d c] [d b a g] |
        [fis g a g] [fis e d c] | [b a g8] r4 |
        \stemup [<g8 d' b' g'> g'' g g] \stemboth | 
	\times 2/3 {[g8 d c]} \times 2/3 {[b a g]} |
	\times 2/3 {[f d c]} \times 2/3 {[b a g]} | [e' c] r4 |
%78
        \stemup [<a8 e' cis' a'> a'' a a] \stemboth |
        \times 2/3 {[a8 e d]} \times 2/3 {[cis b a]} | 
        \times 2/3 {[g e d]} \times 2/3 {[cis b a]} | [fis' d] r4 |
	\times 2/3 {[e'8 fis g]} \times 2/3 {[a b c]} | 
	\times 2/3 {[c b c]} \times 2/3 {[e, d c]} |
        \times 2/3 {[d e fis]} \times 2/3 {[g a b]} | 
	\times 2/3 {[b a b]} \times 2/3 {[d, c b]} | 
	\times 2/3 {[c d e]} \times 2/3 {[fis g a]} |
        \times 2/3 {[a g a]} \times 2/3 {[c, b a]} |
%88
        [b16 g a g] [a g a g] | [c g a g] [a g a g] | [d' g, a g] [a g a g] |
        [e' g, a g] [a g a g] | [fis' d e d] [e d e d] | [g d e d] [e d e d] |
        [a' d, e d] [e d e d] | [b' d, e d] [e d e d] |
%96
        \times 2/3 {[c'8 e, d]} \times 2/3 {[c b a]} | 
	\times 2/3 {[d g, fis]} \times 2/3 {[g fis g]} |
        \stemup \times 2/3 {[d c' c]} \times 2/3 {[c b a]} \stemboth | 
	\times 2/3 {[d g, fis]} \times 2/3 {[g fis g]} |
%100
        [e'16 c b a] [d b a g] | [c a g fis] [b g fis e] | cis2 | d4 c'! |
        [b16 b a g] a4^\trill | g g' |
%106
        r8 r16 cis,, d4 | r8 [b c? d] | [g,16 g'' g g] g4:16 |
        [fis16 g a g] [fis e d c] | [b g' g g] g4:16 |
        [fis16 g a g] [fis e d c] | [b g' g, a] a4^\trill |
        [b16 g' g, a] a4^\trill | [b16 g' g, a] a4^\trill | [g8 d] g,4
%fine

}

viola = \notes \relative c'{
	\context Voice=i
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
	e e | e e | [fis16 g a g] [fis g a g] | 
	[fis8 fis a a] | d,4 b | b g' | cis,? a |
	a fis' b, g' | a a, |
%35
	[a8 fis' fis fis] | r [g g g] | r [a a a] |
	r [b, b b] | r [cis cis cis] | r [d d d] | r [e e e] |
	r [fis fis fis] |
%43
	\times 2/3 {[e g fis]} \times 2/3 {[e fis g]} | fis4 r |
	\times 2/3 {[e8 g fis]} \times 2/3 {[e fis g]} | fis4 r |
%47
	[d8 g d fis] | r [e a, d] | r b [b cis16 d] |
	[e8 d e cis] | [d d d cis] | d4 d' | r8 r16 gis, a4 |
	r8 [fis8 g! a] | d,2  
	[d8 d d d] | [d c!] r4 |
	\times 2/3 {[f8 e d]} \times 2/3 {[c b a]} | [gis a] r4 |
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
	\times 2/3 {[a c b]} \times 2/3 {[a b c]} | b4 r | 
	\times 2/3 {[a,8 a g]} \times 2/3 {[a b c]} |
	b4 r | [g8 c g b] | r [a' d, g] | r e [e fis16 g] | [a8 g a fis] |
%104
	g8 g4 fis8 | g4 g | r8 r16 cis, d4 | r8 [b c? d] | [g, b d e] | 
	[a, a a' a] | [d, b d e] | [a, a a' a] | [d, d e d] | [d d e d] | 
	[d g g fis] | [g d] g,4
%fine 
 
}

bassocontinuo = \notes \relative c'{
	\context Voice=i
	\clef "bass";
	[g16 a b a] [g a b a] | [g8 g, g g'] |
	[g16 a b a] [g a b a] | [g8 g, g g'] | 
%5
	[g16 a b a] [g a b a] | [g8 g,] r4 |
	[c'8 c,] [e fis] | [g g g b,] |
	[c c] [e fis] | [g g b c] |
%11
	[d d d c] | [b b b b] | [fis fis fis fis] |
	[g g b c] | [d fis, fis fis] | [g16 g fis e] [d c b a] |
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
	\times 2/3 {[f8 e d]} \times 2/3 {[c b a]} | [gis a] r4 |
%60
	r2 | r |
	[f'8 d g g,] | [d' d'] [d16 c b a] |
%64 : reprise
	[g16 a b a] [g a b a] | [g8 g, g g'] |
        [g16 a b a] [g a b a] | [g8 g, g g'] |
%68
        [g16 a b a] [g a b a] | g4 r |
	[c8 c,] [e fis] | [g g b, c] | [e fis fis fis] |
%73
	[g16 g, a b] [c d e fis] | [g a b a] [g a b a] | g4 r |
	b, b | [c8 c'16 d] [e d cis b] | [a b cis b] [a b cis b] |
%79
	a4 r | cis,4 cis | [d8 d'16 e] [d8 b] |	
	c4 c | a a | b b | g g | a a | fis fis | g2:8 | a2:8 |
%90
	[b8 b, b b] | c2:8 | d2:8 | e2:8 | fis2:8 | g2:8 | 
	fis4 fis | g r | fis fis | g r | c8 r b r | a r g r |
	r [a a g] | [fis e fis d] | [g, b c d] | g,4 g' |
%106
	r8 r16 cis, d4 | r8 [b c? d] | [g, g' b c] | [d fis, fis fis] |
	[g g, b c] | [d fis fis fis] | [g b, c d] | [g, b c d] | 
	[g, b c d] | [g d] g,4
%fine
}


\score{
        \context StaffGroup <
                \context Staff = i < %\tempi 
					\global \dynamics \violinoi >
                \context Staff = ii < \global \dynamics \violinoii >
                \context Staff = iii < \global \dynamics \viola >
                \context Staff = iv < \global \dynamics \bassocontinuo >
        >
        \header{
		piece="I. Allegro";
	}
	\paper{
                \translator { \OrchestralScoreContext }
	}
        \midi{ \tempo 4 = 100; }
}

\version "1.2.0";
