\header{
filename =	 "standchen.ly";
title    = "St\"andchen";
subtitle = "(Serenade)\\\``Leise flehen meine Lieder''";
opus =	 "D. 957 No. 4";
composer =	 "Franz Schubert (1797-1828)"
	 "Text by Ludwig Rellstab (1799-1860)";
enteredby =	 "JCN";
copyright =	 "public domain";
} 

%{
 Tested Features@ multivoice, accents, lyrics, chords, piano music,
multiple \paper{}s in one \score 
%}

\version "0.1.15";

$vocal_verse1 = \melodic{
	\octave c';
	
	% ugh: treble/bass
%	\clef treble;
%	\clef violin;
	[/3 g8( )as] g \] c'4. g8 |
	[/3 f8( )g] f \] c'4 f8 r |
	g4.-> f8 [/3 f( )es] d \] |
	es2 r4 |
	% ugh: a whole should be a measure
	%r1 |
	r2. |
	r2. |
	[/3 g8( )as] g \] es'4. g8 |
	[/3 f8( )g] f \] d'4. c'8 |
	bes4. as8 [/3 as( )g] f \] |
	g2 r4 |
	r2. |
	r2. |
	g8. b16 es'4. d'8 |
	c'8. g16 es4. c8 |
	% [/3 as\grace( bes ] )
	[/3 as8( )g] as \] c'4. as8 |
	g2. |
	%[/3 f\grace( g] )
	[/3 f8( )e] f \] as4. f8 |
	es!2. |
	g8. b16 es'4. d'8 |
	c'8. g16 e4. c8 |
	% [/3 a\grace( b] )
	[/3 a!8( ) gis] a \] c'4. a8 |
	g!2. |
	% [/3 a\grace( b] )
	[/3 d'8\f cis'] d' \] f'4. b8 |
	c'!2. |
}

$vocal_through = \melodic{
	\octave c';
	g8. g16 b8. b16 d'8. d16 |
	c'4 b r |
	g4. b8 d'8. c'16 |
	b2 r4 |
	e'4. d'8 [/3 d'( )c'] b \] |
	a8. b16 c'4-> a8 r |
	r2. |
	r2. |
	% 4 bars copied from end verse 1
	% [/3 a\grace( b] )
	[/3 a!8( ) gis] a \] c'4. a8 |
	g!2. |
	% [/3 a\grace( b] )
	[/3 d'8\f cis'] d' \] f'4. b8 |
	c'!2. ~ |
	c'4 r c' |
	as2. |
	g |
	es2 r4 |
}

$lyric_verse1 = \lyric{
% 5
	\[/3 Lei-4 se8 \] fleh-4. en8 |
	\[/3 mei-4 ne8 \] Lie-4 der8 _8 |
	Durch4. die8 \[/3 Nacht4 zu8 \] |
	dir;2 _4 |
	_4 _ _ |
	_ _ _ |
% 11
	\[/3 In4 den8 \] stil-4. len8 |
	\[/3 Hainr4 her-8 \] nie-4. der,8 |
	Lieb4. chen,8 \[/3 komm4 zu8 \] |
	mir!2 _4 |
	_4 _ _ |
	_ _ _ |
% 17
	Fl\"us-8. ternd16 schlan-4. ke8 |
	Wip-8. fel16 rau-4. schen8 |
	\[/3 In4 des8 \] Mon-4. des8 |
	Licht;2. |
	_4 _ _ |
	_4 _ _ |
% 23 
	Des8. Ver-16 r\"a-4. ters8 |
	feind-8. lich16 Lau-4. schen8 |
	\[/3 F\"urch-4 te,8 \] Hol-4. de,8 |
	nicht,2. |
	\[/3 f\"urch-4 te,8 \] Hol-4. de,8 |
	nicht.2. |
}
	
$lyric_verse2 = \lyric{
% 5
	\[/3 H\"orst4 die8 \] Nach-4. ti-8 
	\[/3 gal-4 len8 \] schla-4 gen?8 _8
	Ach!4. sie8 \[/3 fleh-4 en8 \] 
	dich,2 _4
	_4 _ _ 
	_4_ _

% 11
	\[/3 Mit4 der8 \] T\"o-4. ne8
	\[/3 s\"u\ss-4 en8 \] Kla-4. gen8
	Fleh-4. en8 \[/3 sie4 f\"ur8 \]
	mich.2 _4
	_4_ _ 
	_4_ _

% 17
	Sie-8. ver-16 stehn4. des8
	Bus-8. ens16 Seh-4. nen,8
	\[/3 Ken-4 nen8 \] Lieb-4. es-8 
	schmerz,2.
	\[/3 Ken-4 nen8 \] Lieb-4. es-8 
	schmerz,2.

% 23
	R\"uh-8. ren16 mit4. den8 
	Sil-8. ber-16 t\"o-4. nen8
	\[/3 Jed-4 es8 \] wei-4. che8 
	Herz.2.
	\[/3 Jed-4 es8 \] wei-4. che8 
	Herz.2.
}

$lyric_through = \lyric{
% 37
	La\ss8. auch16 dir8. die16 Brust8. be-16 |
	we-4 gen, _ |
	Lieb-4. chen,8 h\"o-8. re16 |
	mich!2 _4 |
	Be-8. bend16 harr'4 ich8 _8 |
	dir8. ent-16 ge-4 gen!8 _8 |
	\[/3 Komm,4 be-8 \] gl\"u4. cke8 |
	mich!2. |
	\[/3 Komm,4 be-8 \] gl\"u4. cke8 |
	mich!2. |
	_2 be-4 |
	gl\"u-2. |
	cke2. |
	mich!2 _4 |
}

$treble_intro = \melodic{
	\octave c';
	\clef violin;
	% ugh: i'd like to type this!
	%r8\pp [<g,-. c-.> <c-. es-.> <g,-. c-.> <c-. es-.> <g,-. c-.>] |
	r8\pp <[g,-. c-.> <c-. es-.> <g,-. c-.> <c-. es-.> <g,-. c-.]> |
	r8 <[as,-. c-.> <c-. es-.> <as,-. c-.> <c-. es-.> <as,-. c-.]> |
	r8 <[as,-. c-.> <c-. d-.> <as,-. c-.> <c-. d-.> <as,-. c-.]> |
	r8 <[g,-. b,-.> <b,-. d-.> <g,-. b,-.> <b,-. d-.> <g,-. b,-.]> |
	\break
	
}

$treble_verse1 = \melodic{
	\octave c';
%	\clef violin;
	r8 <[g, c> <c es> <g, c> <c es> <g, c]> |
	r8 <[f, c> <c d> <f, c> <c d> <f, c]> |
	r8 <[f, g, b,> <g, b, d> <f, g, b,> <g, b, d> <f, g, b,]> |
	r8 <[es, g, c> <g, c es> <es, g, c> <g, c es> <es, g, c]> |
	<g'4.( b> <)f'8 d'> <[/3 f' d'> <es' c'> <d' b]1/1> |
	<c'2. es'> |
	r8 <[g, c> <c es> <g, c> <c es> <g, c]> |
	r8 <[f, c> <c d> <f, c> <c d> <f, c]> |
	r8 <[f, as, bes,> <as, bes, d> <f, g, bes,> <g, bes, d> <f, g, bes,]> |
	r8 <[es, g, bes,> <g, bes, es> <es, g, bes,> <g, bes, es]> 
	<{ es'( | )bes4. as8}{ c'( | )d4.( )f8 } > 
	< [/3 f as> <es g> <d f]1/1> |
	<es2. g> |
	r8 <[f, g,> <g, b,> <f, g,> <g, b,> <f, g,]> |
	r8 <[es, g,> <g, c> <es, g,> <g, c> <es, g,]> |
	r8\pp <[es, as, c> <as, c es> <es, as, c> <as, c es> <es, as, c]> |
	r8 <[es, g, bes,> <g, bes, es,> <es, g, bes,> <g, bes, es,> <es, g, bes,]> |
	% [/3 as\grace( bes )
	[/3 as8(( g )as]1/1 c'4.-> ) as8 |
	g2. |
	r8 <[f, g,> <g, b,> <f, g,> <g, b,> <f, g,]> |
	r8 <[e, g,> <g, c> <e, g,> <g, c> <e, g,]> |
	r8 <[f, a, c> <a, c f> <f, a, c> <a, c f> <f, a, c]> |
	r8 <[e, g, c> <g, c e> <e, g, c> <g, c e> <e, g, c]> |
	<{[/3 f'8\f( e' f']1/1 a'4. )f'8 } {\[/3 f e f \] a4. f8 } > |
	<e2 e'> r4 |
}

$treble_eentje = \melodic{
	\octave c';
	<f2\mf as!(> <[as8.->( c'> <)f16 )as]> |
	<e4. g> <[e8-. g-.(> <e-. g-.> <e-. )g-.]> |
	<f4. g> <[b,8-. g-.(> <d-. g-.> <f-. )g-.]> |
	<e2 g\pp> <e4 g> |
	<f2\mf a(> <[a8.( c'> <)f16 )a]> |
	<e4. g> <[e8-. g-.(> <e-. g-.> <e-. )g-.]> |
	<f4. g> <[b,8-. g-.(> <d-. g-.> <f-. )g-.]> |
	<e2. g> |
}

$treble_through = \melodic{
	\octave c';
	r2. |
	% lily: 221: warning: Junking request: Span_dynamic_req: the \>
	<[g,8.\< g> <g,16 g> <b,8. b> <\! b,16\> b16> <d8. d'> <d16 d']> |
	% lily: 222: warning: Can't find cresc to end.
	< { c4( )b, } { c'4( )b } > \!r |
% ugh
%	<g,4. g> <b,8 b> <[d8.-> d'->> c'16] |
	<g,4. g> <b,8 b> [d'8.-> c'16] |
% ugh, ugh: connecting chords
	< { d2.\f( )a2} { e2. ~ e2 } { b2. c2 }> r4 |
	< 
		{
			\voiceone 
			[a8. b16] c'4->( )a8 r |
			[a8. b16] c'4->( )a8 r |
		}
		{ 
			\voicetwo 
			<d4\f f> <d2 f> |
			<c!4\f es> <c2 es> |
		}
	>
	% 4 bars copied from end verse1
	r8 <[f,\p a, c> <a, c f> <f, a, c> <a, c f> <f, a, c]> |
	r8 <[e, g, c> <g, c e> <e, g, c> <g, c e> <e, g, c]> |
	<{[/3 f'8\f( e' f']1/1 a'4. )f'8 } {\[/3 f e f \] a4. f8 } > |
	<e2 e'> r4 |
	<es2 es'> r4 |
	<d2 d'> r4 |
	<b,2 b> r4 |
	<c2 c'> <e4\pp g> |

	% four copied from begin eentje
	<f2\mf as!(> <[as8.->( c'> <)f16 )as]> |
	<e4. g> <[e8-. g-.(> <e-. g-.> <e-. )g-.]> |
	<f4. g> <[b,8-. g-.(> <d-. g-.> <f-. )g-.]> |
	\textstyle "italic";
	<e2._"dim." g> |
	<g,2. e g> |
	<g,2.-\fermata e g> |
}

$bass_intro = \melodic{
	\octave c;
	\clef bass;
	<c,2 c> r4 |
	<as,,2 as,> r4 |
	<f,,2 f,> r4 |
	<g,,2 g,> r4 |
}

$bass_verse1 = \melodic{
	\octave c;
%	\clef bass;
	<c,2 c> r4 |
	<as,,2 as,> r4 |
	<g,,2 g,> r4 |
	<c,2 c> r4 |
	<g,,8 g,> <[g d'> <d' f'> <g d'> <d' f'> <g d']> |
	<c,8 c> <[g c'> <c' es'> <g c'> <c' es'> <g c']> |
	<c,2 c> r4 |
	<as,,2 as,> r4 |
	<bes,,2 bes,> r4 |
	<es,,2 es,> r4 |
	bes,,8 <[bes, f> <f bes> <bes, f> <f bes> <bes, f]> |
	es,,8 <[bes, es> <es g> <bes, es> <es g> <bes, es]> |
	<g,,2 g,> r4 |
	<c,2 c> r4 |
	<as,,2 as,> r4 |
	<es,,2 es,> r4 |
	<bes,,8 bes,> <[f bes> <bes d'> <f bes> <bes d'> <f bes]> |
	<es,,8 es,> <[es g bes> <g bes es'> <es g bes> <g bes es'> <es g bes]> |
	<g,,2 g,> r4 |
	<c,2 c> r4 |
	<f,,2 f,> r4 |
	<c,2 c> r4 |
	<g,,8 g,> <[d g> <g b> <d g> <g b> <d g]> |
	c,8 <[c e g> <e g c'> <c e g> <e g c'> <c e g]> |
}

$bass_eentje = \melodic{
	\octave c;
	<c,8 c> <[c f as!> <f as c'> <c f as> <f as c'> <c f as]> |
	c,8 <[c e g> <e g c'> <c e g> <e g c'> <c e g]> |
	<g,,8 g,> <[d g> <g b> <d g> <g b> <d g]> |
	c,8 <[e g> <g c'> <e g> <g c'> <e g]> |
	<c,8 c> <[c f a> <f a c'> <c f a> <f a c'> <c f a]> |
	c,8 <[c e g> <e g c'> <c e g> <e g c'> <c e g]> |
	<g,,8 g,> <[d g> <g b> <d g> <g b> <d g]> |
	c,8 <[e g> <g c'> <e g> <g c'> <e g]> |
}

$bass_through = \melodic{
	\octave c;
	<g,,8 g,> <[g, b, d> <b, d f> <g, b, d> <as,!-> b-> d->> <b, d f]> |
	<g,,8 g,> <[g, d> <d f> <g, d> <as,-> b-> d->> <b, d f]> |
	% copied
	<g,,8 g,> <[g, d> <d f> <g, d> <as,-> b-> d->> <b, d f]> |
	<g,,8 g,> <[g, d e> <d f> <g, d> <gis,-> b,-> d->> <b, d f]> |
	<gis,,8 gis,> <[d e> <e b> <d e> <e b> <d e]> |
	<a,,8 a,> <[c e> <e a> <c e> <e a> <c e]> |
	<a,,8 a,> <[a, d f> <d f a> <a, d f> <d f a> <a, d f]> |
	<a,,8 a,> <[a, c e> <c e a> <a, c e> <c e a> <a, c e]> |
	% 4 bars copied from end verse1
	<f,,2 f,> r4 |
	<c,2 c> r4 |
	<g,,8 g,> <[d g> <g b> <d g> <g b> <d g]> |
	c,8 <[c e g> <e g c'> <c e g> <e g c'> <c e g]> |

	<c,8 c> <[c es! g> <es g c'> <c es! g> <es g c'> <c es! g]> |
	<f,,8 f,> <[d f> <f as!> <d f> <f as!> <d f]> |
	<g,,8 g,> <[d f> <f g> <d f> <f g> <d f]> |
	c,8 <[c e> <e g> <c e> <e g> <c e]> |
	c,8 <[c f> <f as> <c f> <f as> <c f]> |
	c,8 <[c e> <e g> <c e> <e g> <c e]> |
	g,,8 <[g, d> <d f> <g, d> <d f> <g, d]> |
	% copied from two bars back
	c,8 <[c e> <e g> <c e> <e g> <c e]> |
	c,8 <[c e> <e g> <c e> <e g> <c e]> |
	<c,2._\fermata g, c> |
}
		
global = \melodic{
	\meter 3/4; 
	\key bes es as;
	\skip 4 * 12;
	\break
	\skip 4 * 234;
	\bar "|.";
}


$lyric_four = \lyric{ 
	_4 _ _
	_ _ _
	_ _ _
	_ _ _
}

lyrics = \melodic{
	\meter 3/4; 
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
	\$lyrics
>
		
vocals = \melodic{
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

% treble = \melodic{
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

bass = \melodic{
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

$grand_staff = \type Grand_staff<
	\$treble_staff
	\$bass_staff
>

a4 = \paper{
	linewidth= 193.\mm;
	\include "score-bar-numbering.ly";
}

\score{
	<
		\$vocal_staff
		\$lyric_staff
		\$grand_staff
	>
	\paper{ \a4 }
	\midi{
		\tempo 4 = 54;
	}
}



