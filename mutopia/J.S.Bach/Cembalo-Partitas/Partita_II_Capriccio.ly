\header{
	enteredby = "Tom Cato Amundsen";
	copyright = "Public Domain";
	piece = "Capriccio";
}

%% I takt 22 foretar jeg et stemmekryss mellom stemme en og to, pga at den 
%% stemmen som tidligere har vært andre stemme, kommeri resten av stykket
%% ligge øverst og følgelig er første stemme.
%% 
%% Noe liknende skjer også i mellom andre og tredje stemme i takt 28

\version "1.0.7";

global = \notes{
	\time 2/4;
	\key es;
	s2*48
	\bar ":|:";
	s2*48
	\bar "|.";
}

vOne = \type Voice = voiceOne \notes\relative c''{
	r8 g c4~ |	
	[c8 d16 c][b a g f] |
	[es8 g' d, f'] |
	[es16 d c d][e fis g a] |
%5
	[bes g fis g][d g fis g] |
	[c g fis g][a g fis e] |
	d8 g4 fis8 |
	g r r4 |
%9
	r16 [c, fis, g][a bes c8~] |
	[c16 bes a bes][c d e fis] |
	[g8 fis16 g~] g4~ |
	[g8 e16 g~] g4~ |
	[g8 e16 f~] f4~ |
	[f8~ d16 f~] f4~ |
	[f16 es d f][es d c b] |
	[c b c d][es f g8~]
%17
	[g16 f es g ][f es d c] |
	[b a b c][d es f8~] |
	[f16 as g f][es d c bes!] |
	[as c f es][d c b d] |
%21
	[g f es g][f es d f] |
%stemmekryss ugh
	r16 [c b c][g c b c] |
	[es c a' es][c' es, d c] |
	[d bes a bes][f bes a bes] |
%25
	[d bes g' d][bes' d, c bes] |
	[c a g a][fis a g a] |
	[c a fis' c][a' c, bes a] |
	[bes8 d] g4~ |
%29
	[g8 a16 g][fis e d c] |
	[bes8 d' a, c'] |
	[g, bes'16 a][g8 bes] |
	[e, g bes, e] |
%33
	[f, as'16 g][f8 as] |
	[d, f as, d] |
	es,4 r8 c' |
	as4 r |
%37
	r8 g c4~ |
	[c8 d16 c][b a g f] |
	[es8 g' d, f'~] |
	[f16 es d f][es d c bes] |
%41
	[as g as bes][c d es8~] |
	[es16 d c es][d c bes as] |
	[g f g as][bes c des8~] |
	[des16 c bes des][c bes as g] |
%45
	as2~ |
	[as8 g] g4~ |
	[g8 as16 g][f es d c] |
	g'2 |
%49
	r8 d' g,4~|
	[g8 f16 g][a b c d] |
	[es8 c, f' d,]
	[g'16 as g f][es d c bes] |
%53
	[as8 f' g, e'] |
	[f16 c' e, f] r [c' e, f~] |
	f4 r |
	r8 [f16 g][as bes c d,] |
%57
	es8 r r4 |
	r8 [es16 f][g as bes! g] |
	as8 r r4 |
	r16 [g f es][d c b a] |
%61
	[b c d es] f4~ |
	[f16 es b c] r [es b c] |
	r [d b c] r [c b c~] |
	[c8 g' d f~] |
%65
	[f es16 f] g4~ |
	[g8 f c es~] |
	[es d16 es] f4~ |
	[f8 es16 d][c bes a g] |
	c2~ |
	c4. c8 |
	bes4 a |
	r16 [a' fis g] r16 [a fis g] |
%73
	r8 g es4~ |
	es8 d4 c8~ |
	[c es d c] |
	[bes g'][a, fis'] |
%77
	g4 r8 f! |
	b,4 r |
	r8 g' c,4~ |
	[c8 bes16 c][d e f g] |
%81
	[as16 c8.][as16 c bes c] |
	[as c8.][as16 bes as bes] |
	[g bes8.][g16 bes as bes] |
	[g bes8.][g16 as g as] |
%85
	[f as8.][f16 as g as] |
	[f as8.][f16 g f g] |
	[es g d g][c, f bes, es] |
	[as, c f es] f4 |
%89
	r8 c g'4~ |
	[g8 as16 g][f es d c] |
	b4 r8 es |
	c4 r |
%93
	r8 g f'4~ |
	[f8 d g <f as,]> |
	<es4 g,> <d f,> |
	<c2 g es>
}

vTwo = \type Voice = voiceTwo \notes\relative c{
	es4 r8 es |
	f4 r8 bes |
	c4. b8 |
	c r r4 |
%5
	r8 d g4~ |
	[g8 a16 g][fis e d c] | 
	[bes8 d' a, c'~] |
	[c16 bes a c][bes a g f!] |
%9
	[es8 c] r8 fis! |
	g r r a! |
	[bes16 d8.][bes16 des c des] |
	[bes16 des8.][bes16 c bes c] |
%13	
	[as c8.][as16 c bes c] |
	[as bes8.][as16 bes as bes] |
	g8 r r4 |
	r8 [es16 f][g a b! c] |
%17
	d8 r r4 |
	r8 [d,16 es][f g a! b!] |
	c8 r r4 |
	r2 |
%21
	r8 c4 b8 |
	es4 r |
	s2*5 |
	r16 [g,, fis g][d g fis g]
%29
	[c, g' fis g][a g fis e] |
	d8 g4 fis8 |
	r16 [g fis g][d g fis g] |
	[bes g e' bes][g' bes, as g] |
%33
	[as f e f][c f e f] |
	[as f d' as][f' as, g f] |
	[g8 bes] es4~
	[es8 f16 es][d c bes as] |
%37
	g4 r8 c |
	f4 r |
	r8 [es, f g~] |
	[g g] c4~ |
%41
	[c8 c16 d][es f g as] |
	bes8 f, bes4~ | % HACK vil bruke [...] her
	[bes8 bes16 c][des es f g] |
	as8 [g, f e] |  % HACK vil bruke [...] her
%45
	[f16 f' e g][f es d! c] |
	b4 r8 es |
	c2 |
	b2 |
%49
	b4 r8 d |
	g, c4 b8 |
	r16 [g as g] r [f g f] |
	es8 g' c,4~ | % HACK vil egentlig bruke [...]
%53
	[c8 bes16 c][d e f g] |
	[as8 f,][bes' g,] |
	[c'16 des c bes][as g f es!] |
	[d!16 c d es][f g as8~] |
%57
	[as16 c bes as][g f es d] |
	[c b c d][es f g8~] |
	[g16 bes as g][f es d c] |
	b8 r r4 |
%61
	r4 r8 d'8 |
	g,4 f |
	es d |
	[es8 es' b d~] |
%65
	d4. [c16 bes] |
	a4. c8 |
	c4. [bes16 as] |
	g4 r |
%69
	r8 [bes as g] |
	[fis a] d,4~ |
	[d8 c16 d][e fis g a!] |
	[bes8 g,][c' a,]
%73
	d'4. c8 |
	fis, bes4 a8~ |
	[a fis g a] |
	d, r r16 [es' d c] |
%77
	[b8 d] g,4~ |
	[g8 f16 g][a b! c d] |
	[es b, c d][es f g as!] |
	[bes! as g as][bes c d e!] |
%81
	[f8 e16 f~] f4~ |
	[f8 e16 f~] f4~ |
	[f8 d16 es!~] es4~ |
	[es8 d16 es~] es4~ |
%85
	[es8 c16 d~] d4~ |
	[d8 c16 d~] d4 |
	[c8 bes! as g] |
	f4 r16 [c' b d] |
%89
	g,4 r8 es' |
	c4 r |
	r8 d, g4~ |
	[g8 as16 g][f es d c] |
%93
	b4 r8 d' |
	[g, b c d~] |
% ???
% What have i misunderstood? I want the note_head not to collide with
% i hotehead i vOne
	\property Voice.hshift = -1
	d 
	\property Voice.hshift = 0
	c4 b8 |
}
vThree = \type Voice = voiceThree \notes\relative c{
	\stemdown
	c4 r8 c |
	d4 r |
	r8 [es f g] |
	[c, c' bes! a] |
%5
	g4 r8 g |
	a4 r |
	r8 [bes, c d] |
	[g, d'] g4~ |
%9
	[g8 a16 g][fis e d c] |
	[bes8 d' a, c'] |
	[g, bes' f,! as'!] |
	[e, g' c,, e']
%13
	[f, as' es,! g'] |
	[d, f' bes,, d'] |
	[es, es' f g] |
	as r8 r4 |
%17
	r8 [d, es f] |
	g r8 r4 |
	r8 g c4~ |
	[c8 d16 c][b a g f] |
%21
	[es8 g' d, f'] |
	[c, es'16 d][c8 es] |
	[a, c f, a] |
	[bes, d'16 c][bes8 d] |
%25
	[g, bes es, g] |
	[a, c'16 bes][a8 c] |
	[fis, a d, fis] |
	g,8 r bes r |
%29
	a r c r |
	r [bes c d] |
	g, r bes r |
	r4 c8 r |
%33
	f r as, r |
	r4 bes8 r |
	r16 [f'16 es d][c bes as g] |
	[f es' d c][bes as g f] |
%37
	[es d' c bes][as g f es] |
	[d c' b a][g f es d] |
	c8 c'4 b8 |
	c4 r16 [c d es] |
%41
	f8 r r4 |
	bes,4 r16 [bes c d] |
	es8 r r4 |
	r8 [as, bes c] |
%45
	[f, c'] f4~ |
	[f16 es d f][es d c bes] |
	[as8 g] as4^\mordent |
	g2
%49
	g'4 r8 f |
	es4 d |
	c8 r d r |
	es4 r8 e |
%53
	f4 [bes8 c] |
	f8 r g r |
	[as f,g as] |
	bes r r4 |
%57
	r8 [es, f g] |
	as r r4 |
	r8 [d, es f] |
	[g d] g,4~ |
%61
	[g8 f16 g][a b c d] |
	[es8 c, f' d,] |
	[g' es, as' f,] |
	[g'16 g, g' as][g f es d] |
%65
	[c g' c d][c bes a g] |
	[f f, f' g][f es d c] |
	[bes f' bes c][bes as! g f] |
	[es es, es' f][es d c bes] |
%69
	[a g a bes][c d es c] |
	[d c d e][fis g a fis] |
	[g8 es c d] |
	g, r es' r |
%73
	[bes16 g' fis g][c, a' fis g] |
	[d bes' fis g][es c' fis, g] |
	[fis, es' d c][bes a g fis] |
	[g f! es d][c8 d] |
%77
	[g16 fis g a][b c d es] |
	[f es d es][f g a b] |
	c8 r r c |
	e, r r c' |
%81
	[f, as' es, g'] |
	[d, f' bes,, d'] |
	[es, g' d, f'] |
	[c, es' as,, c'] |
%85
	[d,8 f' c, es'] |
	[b, d' g,, b'] |
	[c, g'] c4~ |
	[c8 d16 c][b a g f] |
%89
	[es d c d][es f g es] |
	[as g f g] as4~ |
	[as16 as g f][es d c bes!] |
	[as g' f es][d c b a] |
%93
	[g es' d c][b a g f] |
	[es as g f][es d es f] |
	[g c es fis][g8 g,] |
	<c2 c,>
}

vOneSwitch = \type Voice = voiceOne \notes{
	s2*4
%5
	\stemup	s2*17
%22
	\stemdown s16*1
	\stemboth s16*7
%23
	s2*12 
%35
	s4 \stemup s4 
	s2*3
%39
	\stemboth s2*2
%41
	\stemup s2*8
%49
	\stemboth s2*3
%52
	\stemup s2*8
%60
	\stemboth s2
%61
	\stemup s2*35
%96
	\stemboth
}

vTwoSwitch = \type Voice = voiceTwo \notes{
	\stemup	s2*4 
%5
	\stemdown \translator Staff = treble s2*17
%22
	\stemup
	s2*6
%28
	\translator Staff = bass s2*7
%35
	s4 \translator Staff = treble \stemdown s4
	s2*3
%39	
	\translator Staff = bass \stemup s2*2
%41
	s4 \translator Staff = treble \stemdown s4
%42
	s8 \translator Staff = bass \stemup s8*3 
%43
	s4 \translator Staff = treble \stemdown s4
%44
	s8 \translator Staff = bass \stemup s8*3
%45
	\translator Staff = treble \stemdown s2*4
%49
	\stemup \translator Staff = bass s2*3
%52
	s8*1 \stemdown \translator Staff = treble \stemdown s8*3
	s2*7
%60
	\stemup \translator Staff = bass s2
%61
	\translator Staff = treble \stemdown s2*35
}

vThreeSwitch = \type Voice = voiceThree \notes{
	\stemdown s2*6
%7
	\stemboth s2*21
%28
	\stemdown s2*7
%35
	s4 \stemboth s4
%36
	s2*3
%39
	\stemdown s2*7
%46
	\stemboth s2*3
%49
	\stemdown s2*3
%52
	s4 \stemboth s4
	s2
%54
	s2*2 %HACK evt endre her, sammenlikne med Henle	
%56
	s2*4
%60
	\stemdown 
}

\score{
	\type GrandStaff<
		\type Staff = treble <
			\global
			\vOneSwitch
			\vOne
		>
		\type Staff = bass <			
			\clef bass;
			\global
			\vTwoSwitch
			\vTwo
			\vThreeSwitch
			\vThree
		>
	>
	\paper{
	}
	\midi{\tempo=60;}
}
