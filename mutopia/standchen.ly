\header{
filename =	 "standchen.ly";
title    = "St\\\"andchen";
subtitle = "(Serenade)\\\\``Leise flehen meine Lieder''";
opus =	 "D. 957 No. 4";
date = "August 1828";
composer =	 "Franz Schubert (1797-1828)";
poet=	 "Text by Ludwig Rellstab (1799-1860)";
enteredby =	 "JCN";
copyright =	 "public domain";
} 

%{
 Tested Features@ multivoice accents lyrics chords piano music
multiple \paper{}s in one \score 
Note: Original key F.
%}

\version "1.0.7";

$vocal_verse1 = \notes\relative c{
	% ugh: treble/bass
%	\clef treble;
%	\clef violin;
	\property Voice.dynamicdir=1
	[/3 g''8( )as] g \]/1 c4. g8 |
	[/3 f8( )g] f \]/1 c'4 f,8 r |
	g4.-> f8 [/3 f( )es] d \]/1 |
	es2 r4 |
	% ugh: a whole should be a measure
	%r1 |
	R2. |
	R2. |
	[/3 g8( )as] g \] es'4. g,8 |
	[/3 f8( )g] f \] d'4. c8 |
	bes4. as8 [/3 as( )g] f \] |
	g2 r4 |
	R2. |
	R2. |
	g8. b16 es4. d8 |
	c8. g16 es4. c8 |
	% [/3 as\grace( bes ] )
%	\tiny  [as'16*1/16 bes16*1/16 ] 
%	\normalsize \[/3 [as8 g8] as8 \]31/32 c4. as8 |
	[/3 as'8( )g] as \] c4. as8 |
	g2. |
	%[/3 f\grace( g] )
	[/3 f8( )e] f \] as4. f8 |
	%\tiny \[/3 [f`16*1/16 g16*1/16] \normalsize
	es!2. |
	g8. b16 es4. d8 |
	c8. g16 e4. c8 |
	% [/3 a\grace( b] )
	[/3 a'!8( ) gis] a \] c4. a8 |
	g!2. |
	% [/3 a\grace( b] )
	[/3 d'8\f cis] d \] f4. b,8 |
	c!2. |
}

$vocal_through = \notes\relative c{
	\property Voice.dynamicdir=1
	g''8. g16 b8. b16 d8. d16 |
	c4 b r |
	g4. b8 d8. c16 |
	b2 r4 |
	e4. d8 [/3 d( )c] b \] |
	a8. b16 c4-> a8 r |
	R2. |
	R2. |
	% 4 bars copied from end verse 1
	% [/3 a\grace( b] )
	[/3 a!8( ) gis] a \] c4. a8 |
	g!2. |
	% [/3 a\grace( b] )
	[/3 d'8\f cis] d \] f4. b,8 |
	c!2. ~ |
	c4 r c |
	as2. |
	g |
	e2 r4 |
}

$lyric_verse1 = \lyrics{
% 5
	\[/3 Lei-4 se8 \] fleh-4. en8 |
	\[/3 mei-4 ne8 \] Lie-4 der8 _8 |
	Durch4. die8 \[/3 Nacht4 zu8 \] |
	dir;2 _4 |
	_4 _ _ |
	_ _ _ |
% 11
	\[/3 In4 den8 \] stil-4. len8 |
	\[/3 Hain4 her-8 \] nie-4. der8 |
	Lieb-4. chen,8 \[/3 komm4 zu8 \] |
	mir!2 _4 |
	_4 _ _ |
	_ _ _ |
% 17
	Fl\"us-8. ternd16 schlan-4. ke8 |
	Wip-8. fel16 rau-4. schen8 |
	\[/3 In4 des8 \] Mon-4. des8 |
	Licht;2. |
	\[/3 In4 des8 \] Mon-4. des8 |
	Licht;2. |
% 23 
	Des8. Ver-16 r\"a-4. thers8 |
	feind-8. lich16 Lau-4. schen8 |
	\[/3 F\"urch-4 te8 \] Hol-4. de8 |
	nicht2. |
	\[/3 f\"urch-4 te8 \] Hol-4. de8 |
	nicht.2. |
}
	
$lyric_verse2 = \lyrics{
% 5
	\[/3 H\"orst4 die8 \] Nach-4. ti-8 
	\[/3 gal-4 len8 \] schla-4 gen?8 _8
	ach!4. sie8 \[/3 fleh-4 en8 \] 
	dich,2 _4
	_4 _ _ 
	_4_ _

% 11
	\[/3 Mit4 der8 \] T\"o-4. ne8
	\[/3 s\"u-4 "\ss{}en"8 \] Kla-4. gen8
	Fleh-4. en8 \[/3 sie4 f\"ur8 \]
	mich.2 _4
	_4_ _ 
	_4_ _

% 17
	Sie-8. ver-16 stehn4. des8
	Bus-8. ens16 Seh-4. nen8
	\[/3 Ken-4 nen8 \] Lieb-4. es-8 
	schmerz,2.
	\[/3 Ken-4 nen8 \] Lieb-4. es-8 
	schmerz.2.

% 23
	R\"uh-8. ren16 mit4. den8 
	Sil-8. ber-16 t\"o-4. nen8
	\[/3 jed-4 es8 \] wei-4. che8 
	Herz,2.
	\[/3 jed-4 es8 \] wei-4. che8 
	Herz.2.
}

$lyric_through = \lyrics{
% 37
	La\ss8. auch16 dir8. die16 Brust8. be-16 |
	we-4 gen _ |
	Lieb-4. chen,8 h\"o-8. re16 |
	mich!2 _4 |
	Be-4. bend8 \[/3 harr'4 ich8\]/1 |
	dir8. ent-16 ge-4 gen!8 _8 |
	_2. |
	_2. |
	\[/3 Komm4 be-8 \] gl\"u4. cke8 |
	mich!2. |
	\[/3 Komm4 be-8 \] gl\"u4. cke8 |
	mich!2. |
	_2 be-4 |
	gl\"u-2. |
	cke2. |
	mich!2 _4 |
}

$treble_intro = \notes\relative c{
	\clef violin;
	% ugh: id like to type this!
	%r8\pp [<g'-. c-.> <c-. es-.> <g-. c-.> <c-. es-.> <g-. c-.>] |
	r8^"\bf m\\\"assig"\pp <[g'-. c-.> <c-. es-.> <g-. c-.> <c-. es-.> <g-. c-.]> |
	r8 <[as-. c-.> <c-. es-.> <as-. c-.> <c-. es-.> <as-. c-.]> |
	r8 <[as-. c-.> <c-. d-.> <as-. c-.> <c-. d-.> <as-. c-.]> |
	r8 <[g-. b-.> <b-. d-.> <g-. b-.> <b-. d-.> <g-. b-.]> |
	\break
}

$treble_verse1 = \notes\relative c{
%	\clef violin;
	%5
	r8 <[g' c> <c es> <g c> <c es> <g c]> |
	r8 <[f c'> <c' d> <f, c'> <c' d> <f, c']> |
	r8 <[f g b> <g b d> <f g b> <g b d> <f g b]> |
	r8 <[es g c> <g c es> <es g c> <g c es> <es g c]> |
	<g''4.( b,> <)f8 d> <[/3 f( d> <es c> <)d b]1/1> |
	%10
	<c2. es> |
	r8 <[g, c> <c es> <g c> <c es> <g c]> |
	r8 <[f c'> <c' d> <f, c'> <c' d> <f, c']> |
	r8 <[f as bes> <as bes d> <f g bes> <as bes d> <f g bes]> |
	r8 <[es g bes> <g bes es> <es g bes> <g bes es]> 
	<{ es'~ | d4. ~ f8}{ c'~ | bes4. ~ as8 } > 
	< [/3 f( as> <es g> <)d f]1/1> |
	%16
	<es2. g> |
	r8 <[f, g> <g b> <f g> <g b> <f g]> |
	r8 <[es g> <g c> <es g> <g c> <es g]> |
	r8\pp <[es as c> <as c es> <es as c> <as c es> <es as c]> |
	%20
	r8 <[es g bes> <g bes es> <es g bes> <g bes es> <es g bes]> |
	% [/3 as\grace( bes )
	[/3 as'8( g as]1/1 c4.-> ) as8 |
	g2. |
	r8 <[f, g> <g b> <f g> <g b> <f g]> |
	r8 <[e g> <g c> <e g> <g c> <e g]> |
	r8 <[f a c> <a c f> <f a c> <a c f> <f a c]> |
	r8 <[e g c> <g c e> <e g c> <g c e> <e g c]> |
	<{[/3 f'8\f( e f]1/1 a4.-> )f8 } {\[/3 f' e f \]/1 a4. f8 } > |
}

$treble_eentje = \notes\relative c'{
	<e2 e'> <e4 g>|
	<f2\mf as!(> <[as8.->( c> <)f16 )as]> |
	<e4. g> <[e8-. g-.(> <e-. g-.> <e-. )g-.]> |
	<f4. g> <[b,8-. g'-.(> <d-. g-.> <f-. )g-.]> |
	<e2 g> <e4\pp g> |
	<f2 a(> <[a8. c> <f16 )a]> |
	<e4. g> <[e8-. g-.(> <e-. g-.> <e-. )g-.]> |
	<f4. g> <[b,8-. g'-.(> <d-. g-.> <f-. )g-.]> |
	%60
	<e2. g> |
}

$treble_through = \notes\relative c'{
	<e2. e'> |
	%61
	R2. |
	<[g,8.\< g'> <g16 g'> <b8. b'> <\! b16\> b'16> <d8. d'> <d16 d']> |
	< { c4( )b } { c'4( )b } > \!r |
% ugh
%	<g4. g> <b8 b> <[d8.-> d->> c16] |
	<g4. g'> <b8 b'> <[ d'8.-> d,-> > c16] |
% ugh ugh: connecting chords
	%65
	< { d,2.\f a'2} { e2. ~ e2 } { b'2. c,2 }> r4 |
	\type Staff < 
		{
			\voiceone 
			[a8. b16] c4-> ~ a8 r |
			[a8. b16] c4-> ~ a8 r |
		}
		{ 
			\voicetwo 
			<d,4 f> <d2 f> |
			<c!4 es> <c2 es> |
		}
	>
	% 4 bars copied from end verse1
	r8 <[f, a c> <a c f> <f a c> <a c f> <f a c]> |
	%70
	r8 <[e g c> <g c e> <e g c> <g c e> <e g c]> |
	<{[/3 f'8\f( e f]1/1 a4.-> )f8 } {\[/3 f' e f \]/1 a4. f8 } > |
	<e2 e'> r4 |
	<es!2 es'! > r4 |
	\property Voice . textstyle =  "italic"
	<d2_"decresc." d'> r4 |
	%75
	<b2 b'> r4 |
	<c2 c'> <e4\pp g> |

	% four copied from begin eentje
	<f2 as!(> <[as8.-> c> <f16 )as]> |
	<e4. g> <[e8-. g-.(> <e-. g-.> <e-. )g-.]> |
	<f4. g> <[b,8-. g'-.(> <d-. g-.> <f-. )g-.]> |
	%80
	\property Voice . textstyle =  "italic"
	<e2._"dim." g> |
	<g,2. e' g> |
	<g2.-\fermata e' g> |
}

$bass_intro = \notes\relative c{
	\clef bass;
	\property Voice.dynamicdir=1
%1
	<c,2 c'> r4 |
	<as2 as'> r4 |
	<f2 f'> r4 |
	<g2 g'> r4 |
}

$bass_verse1 = \notes\relative c{
%	\clef bass;
	\property Voice.dynamicdir=1
%5
	<c,2 c'> r4 |
	<as2 as'> r4 |
	<g2 g'> r4 |
	<c2 c'> r4 |
	<g8 g'> <[g'' d'> <d' f> <g, d'> <d' f> <g, d']> |
%10
	<c,,8 c'> <[g'' c> <c es> <g c> <c es> <g c]> |
	<c,,2 c'> r4 |
	<as2 as'> r4 |
	<bes2 bes'> r4 |
	<es,2 es'> r4 |
%15
	bes'8 <[bes' f'> <f' as> <bes, f'> <f' as> <bes, f']> |
	es,8 <[bes' es> <es g> <bes es> <es g> <bes es]> |
	<g,2 g'> r4 |
	<c2 c'> r4 |
	<as2 as'> r4 |
	<es2 es'> r4 |
	<bes'8 bes'> <[f'' bes> <bes d> <f bes> <bes d> <f bes]> |
	<es,,8 es'> <[es'' g bes> <g bes es> <es g bes> <g bes es> <es g bes]> |
	<g,,2 g'> r4 |
	<c2 c'> r4 |
	<f,2 f'> r4 |
	<c'2 c'> r4 |
	<g8 g'> <[d'' g> <g b> <d g> <g b> <d g]> |
	c,8 <[c' e g> <e g c> <c e g> <e g c> <c e g]> |
}

$bass_eentje = \notes\relative c{
	\property Voice.dynamicdir=1
	<c,8 c'> <[c' f as!> <f as c> <c f as> <f as c> <c f as]> |
	c,8 <[c' e g> <e g c> <c e g> <e g c> <c e g]> |
	<g,8 g'> <[d'' g> <g b> <d g> <g b> <d g]> |
	c,8 <[e' g> <g c> <e g> <g c> <e g]> |
	<c,8 c'> <[c' f a> <f a c> <c f a> <f a c> <c f a]> |
	c,8 <[c' e g> <e g c> <c e g> <e g c> <c e g]> |
	<g,8 g'> <[d'' g> <g b> <d g> <g b> <d g]> |
	c,8 <[e' g> <g c> <e g> <g c> <e g]> |
}

$bass_through = \notes\relative c{
	\property Voice.dynamicdir=1
	%61
	<g,8^"cresc." g'> <[g' b d> <b d f> <g b d> <as!-> b-> d->> <b d f]> |
	<g,8 g'> <[g' d'> <d' f> <g, d'> <as-> b-> d->> <b d f]> |
	% copied
	<g,8 g'> <[g' d'> <d' f> <g, d'> <as-> b-> d->> <b d f]> |
	<g,8 g'> <[g' d' e> <d' f> <g, d'> <gis-> b-> d->> <b d f]> |
	%65
	<gis,8 gis'> <[d''\> e> <e b'> <d e> <e b'> <d\! e]> |
	<a,8 a'> <[c' e> <e a> <c e> <e a> <c e]> |
	<a,8 a'> <[a' d f> <d f a> <a d f> <d f a> <a d f]> |
	<a,8 a'> <[a' c e> <c e a> <a c e> <c e a> <a c e]> |
	% 4 bars copied from end verse1
	<f,2\p f'> r4 |
	%70
	<c'2 c'> r4 |
	<g8 g'> <[d'' g> <g b> <d g> <g b> <d g]> |
	c,8\> <[c' e g> < \! e g c> <c e g> <e g c> <c e g]> |

	<c,8 c'> <[c' es! g> <es g c> <c es g> <es g c> <c es g]> |
	<f,,8 f'> <[d'' f> <f as!> <d f> <f as> <d f]> |
	%75
	<g,,8 g'> <[d'' f> <f g> <d f> <f g> <d f]> |
	c,8 <[c' e> <e g> <c e> <e g> <c e]> |
	c,8 <[c' f> <f as> <c f> <f as> <c f]> |
	c,8 <[c' e> <e g> <c e> <e g> <c e]> |
	<g,8 g'> <[g' d'> <d' f> <g, d'> <d' f> <g, d']> |
	%80
	c,8 <[c' e> <e g> <c e> <e g> <c e]> |
	c,8 <[c' g> <e c> <c g> <e c> <c g]> |
	<c,2._\fermata g' c> |
}
		
global = \notes{
	\time 3/4; 
	\key es;
	\skip 4 * 12;
	\break
	\skip 4 * 234;
	\bar "|.";
}


$lyric_four = \lyrics{ 
	_4 _ _
	_ _ _
	_ _ _
	_ _ _
}
 
allLyrics = {
	\time 3/4; 
%	\skip 4 * 12; 
	\$lyric_four
	\$lyric_verse1
%	\skip 4 * 24; 
	\$lyric_four
	\$lyric_four
	\$lyric_verse2
	\$lyric_through
}

$lyric_staff = \type Lyrics = lyric<
	\allLyrics
>
		
vocals = \notes{
	\property Voice.dynamicdir=UP
	\skip 4 * 12; 
	\$vocal_verse1 
	\skip 4 * 24; 
	\$vocal_verse1
	\$vocal_through
}

$vocal_staff = \type Staff = vocal<
	\property Staff.instrument = "alto sax"
	\global
	\$vocals
>

treble = {
	\$treble_intro 
	\$treble_verse1 
	\$treble_eentje
	\$treble_verse1 
	\$treble_through
}

$treble_staff = \type Staff = treble< 
	\global
	\treble
>

bass = {
	\$bass_intro 
	\$bass_verse1 
	\$bass_eentje
	\$bass_verse1 
	\$bass_through
}

$bass_staff = \type Staff = bass<
	\global
	\bass
>

$grand_staff = \type GrandStaff<
	\$treble_staff
	\$bass_staff
>

\score{
	% Transpose as you like for your voice
	% Range untransposed is c' to f'' (for tenors and sopranos)
	% To get original, \transpose d'
	% \transpose a gives a' to d'' (for basses, who sing an octave down)
	<
		{ \notes \transpose a { \$vocal_staff } }
		\$lyric_staff
		{ \notes \transpose a { \$grand_staff } }
	>
	\paper { \translator { \OrchestralScoreContext }}
	\midi{
		\tempo 4 = 54;
	}
}
