\header{
filename =	 "standchen.ly";
%
% From the album:
% Schubert-Lieder
% Edition Schott No. 608
% Without date, but makes mention of
% ``Jubil"aums-Ausgabe zum 100. Todestage'' ie, 1928
%
% page 60
%
title    = "St\\\"andchen";
subtitle = "(Serenade)\\\\``Leise flehen meine Lieder''";
opus =	 "D. 957 No. 4";
date = "August 1828";
composer =	 "Franz Schubert (1797-1828)";
poet=	 "Text by Ludwig Rellstab (1799-1860)";
enteredby =	 "JCN";
copyright =	 "public domain";

  % mutopia headers.
  mutopiatitle = "St\\\"andchen";
  mutopiasubtitle = "(Serenade)\\\\``Leise flehen meine Lieder''";
  mutopiacomposer = "Franz Schubert (1797-1828)";
  mutopiapoet = "Ludwig Rellstab (1799-1860)";
  mutopiaopus = "D. 957 No. 4";
  mutopiainstrument = "Piano";
  style = "Romantic";
  copyright = "Public Domain";
  tagline =    "\\\\This music is part of the Mutopia project, http://sca.uwater
loo.ca/Mutopia/\\\\It has been typeset and placed in the public domain by Jan Nieuwenhuizen.\\\\Unrestricted modification and redistribution is permitted and enc
ouraged - copy this music and share it!";
  maintainer = "janneke@gnu.org";
  lastupdated = "1999/Oct/17";

} 

%{
 Tested Features: multivoice accents lyrics chords piano music
multiple \paper{}s in one \score 
Note: Original key F.
%}

\version "1.3.4";

vocalVerse = \notes\relative c''{
	\property Voice.dynamicDirectionection=1
	\times 2/3 { [ g8( )as] g } c4. g8 |
	\times 2/3 { [ f8( )g] f } c'4 f,8 r |
	g4.-> f8 \times 2/3 { [ f( )es] d } |
	es2 r4 |
	R2. |
	R2. |
	\times 2/3 { [ g8( )as] g } es'4. g,8 |
	\times 2/3 { [ f8( )g] f } d'4. c8 |
	bes4. as8 \times 2/3 { [ as( )g] f } |
	g2 r4 |
	R2. |
	R2. |
	g8. b16 es4. d8 |
	c8. g16 es4. c8 |
	\grace { as'16 bes } \times 2/3 { [ )as8( )g] as } c4. as8 |
	g2. |
	\grace { f16( g } \times 2/3 { [ )f8( )e] f } as4. f8 |
  
	es!2. |
	g8. b16 es4. d8 |
	c8. g16 e4. c8 |
 	\grace { a'16( b } \times 2/3 { [ )a!8( ) gis] a } c4. a8 |
	g!2. |
	\times 2/3 { [ d'8\f( )cis] d } f4. b,8 |
	c!2. |
}

vocalThrough = \notes\relative c{
	\property Voice.dynamicDirectionection=1
	g''8. g16 b8. b16 d8. d16 |
	c4 b r |
	g4. b8 d8. c16 |
	b2 r4 |
	e4. d8 \times 2/3 { [ d( )c] b } |
	a8. b16 c4-> a8 r |
	R2. |
	R2. |
	\grace { a16( b } \times 2/3 { [ )a!8( ) gis] a } c4. a8 |
	g!2. |
	\times 2/3 { [ d'8\f( )cis] d } f4. b,8 |
	c!2. ~ |
	c4 r c |
	as2. |
	g |
	e2 r4 |
}

lyricVerseOne = \lyrics{
	Lei- se fleh- en mei- ne Lie- der 
	Durch die Nacht zu dir; 
	In den stil- len Hain her- nie- der
	Lieb- chen, komm zu mir! 
	
	Fl\"us- ternd schlan- ke Wip- fel rau- schen
	In des Mon- des Licht,
	In des Mon- des Licht;

	Des Ver- r\"a- ters feind- lich Lau- schen
	F\"urch- te, Hol- de, nicht,
	F\"urch- te, Hol- de, nicht.
}
	
lyricVerseTwo = \lyrics{
	H\"orst die Nach- ti- gal- len schla- gen? 
	Ach! sie fleh- en dich, 
	Mit der T\"o- ne s\"u- "\ss en" Kla- gen
	Fleh- en sie f\"ur mich. 
	
	Sie- ver- "steh'n" des Bus- ens Seh- nen
	Ken- nen Lieb- es- schmerz,
	Ken- nen Lieb- es- schmerz.

	R\"uh- ren mit den Sil- ber- t\"o- nen
	Jed- es wei- che Herz,
	Jed- es wei- che Herz.
}

lyricThrough = \lyrics{
	La\ss auch dir die Brust be- we- gen 
	Lieb- chen, h\"o- re mich! 
	Be- bend harr' ich dir ent- ge- gen! 
	
	Komm, be- gl\"uk- ke mich!
	Komm, be- gl\"uk- ke mich, __ 
	Be- gl\"uk- ke mich!
}

trebleIntro = \notes\relative c{
	r8^"\bf M\\\"a\\ss ig"\pp <g'-. c-.> <c-. es-.> <g-. c-.> <c-. es-.> <g-. c-.> |
	r8 <as-. c-.> <c-. es-.> <as-. c-.> <c-. es-.> <as-. c-.> |
	r8 <as-. c-.> <c-. d-.> <as-. c-.> <c-. d-.> <as-. c-.> |
	r8 <g-. b-.> <b-. d-.> <g-. b-.> <b-. d-.> <g-. b-.> |
	\break
}

trebleVerseOne = \notes\relative c{
	%5
	r8 <g' c> <c es> <g c> <c es> <g c> |
	r8 <f c'> <c' d> <f, c'> <c' d> <f, c'> |
	r8 <f g b> <g b d> <f g b> <g b d> <f g b> |
	r8 <es g c> <g c es> <es g c> <g c es> <es g c> |
	<g''4.( b,> <)f8 d>
	\times 2/3 { < [ f( d> <es c> <)d b] > } |
	%10
	<c2. es> |
	r8 <g, c> <c es> <g c> <c es> <g c> |
	r8 <f c'> <c' d> <f, c'> <c' d> <f, c'> |
	r8 <f as bes> <as bes d> <f g bes> <as bes d> <f g bes> |
	r8 <es g bes> <g bes es> <es g bes> <g bes es> 
	<{ es'( )  d4.() f8}{ c' | bes4.  as8 } > 
	\times 2/3 { < [f( as> <es g> <)d f] > } |
	%16
	<es2. g> |
	r8 <f, g> <g b> <f g> <g b> <f g> |
	r8 <es g> <g c> <es g> <g c> <es g> |
	r8\pp <es as c> <as c es> <es as c> <as c es> <es as c> |
	%20
	r8 <es g bes> <g bes es> <es g bes> <g bes es> <es g bes> |
	\grace { as'16( bes } \times 2/3 { [ )as8( g as] } c4.-> ) as8 |
	g2. |
	r8 <f, g> <g b> <f g> <g b> <f g> |
	r8 <e g> <g c> <e g> <g c> <e g> |
	r8 <f a c> <a c f> <f a c> <a c f> <f a c> |
	r8 <e g c> <g c e> <e g c> <g c e> <e g c> |
	\times 2/3 <
	  { [ f'8\f( e f]  }
	  {  f' e f } >
	< {a4.- > )f8}  { a'4. f8 }  > |
}

trebleEentje = \notes \relative c'{
	\context Voice=one \property Voice.verticalDirection = 0
	<e2 e'> <e4 g>|
	<f2\mf as!(> <as8.->( c> <)f16 )as> |
	<e4. g> <e8-. g-.(> <e-. g-.> <e-. )g-.> |
	<f4. g> <b,8-. g'-.(> <d-. g-.> <f-. )g-.> |
	<e2 g> <e4\pp g> |
	<f2 a(> <a8. c> <f16 )a> |
	<e4. g> <e8-. g-.(> <e-. g-.> <e-. )g-.> |
	<f4. g> <b,8-. g'-.(> <d-. g-.> <f-. )g-.> |
	%60
	<e2. g> |
}

trebleThrough = \notes \relative c'{
	\context Voice=one \property Voice.verticalDirection = 0
	<e2. e'> |
	%61
	R2. |
	[<g,8.\< g'> <g16 g'> <b8. b'> <\! b16\> b'16> <d8. d'> <d16 d'>] |
	< { c4( )b } { c'4( )b } > \!r |

	<g4. g'> <b8 b'> [<d'8.-> d,-> > c16] |
	%65
	< { d,2.\f a'2} { e2. ~ e2 } { b'2. c,2 }> r4 |
	\context Staff < 
		{
			\context Voice=one \property Voice.verticalDirection = 1 
			a8. b16 c4-> () a8 r |
			a8. b16 c4-> () a8 r |
		}
		{ 
			\context Voice=two \property Voice.verticalDirection = -1 
			<d,4 f> <d2 f> |
			<c!4 es> <c2 es> |
		}
	>
	\context Voice=one \property Voice.verticalDirection = 0
	% 4 bars copied from end verse1
	r8 <f, a c> <a c f> <f a c> <a c f> <f a c> |
	%70
	r8 <e g c> <g c e> <e g c> <g c e> <e g c> |
	\times 2/3 < { [ f'8\f( e f] }
	   {  f' e f }>
	< { a4.-> )f8 } { a'4. f8 } > |
	<e2 e'> r4 |
	<es!2 es'! > r4 |
	\property Voice . textStyle =  "italic"
	<d2_"decresc." d'> r4 |
	%75
	<b2 b'> r4 |
	<c2 c'> <e4\pp g> |

	% four copied from begin eentje
	<f2 as!(> <as8.-> c> <f16 )as> |
	<e4. g> <e8-. g-.(> <e-. g-.> <e-. )g-.> |
	<f4. g> <b,8-. g'-.(> <d-. g-.> <f-. )g-.> |
	%80
	\property Voice . textStyle =  "italic"
	<e2._"dim." g> |
	<g,2. e' g> |
	<g2.-\fermata e' g> |
}

bassIntro = \notes\relative c{
	\property Voice.dynamicDirectionection=1
%1
	<c,2 c'> r4 |
	<as2 as'> r4 |
	<f2 f'> r4 |
	<g2 g'> r4 |
}

bassVerseOne = \notes\relative c{
%	\clef bass;
	\property Voice.dynamicDirectionection=1
%5
	<c,2 c'> r4 |
	<as2 as'> r4 |
	<g2 g'> r4 |
	<c2 c'> r4 |
	<g8 g'> [<g'' d'> <d' f> <g, d'> <d' f> <g, d'>] |
%10
	<c,,8 c'> [<g'' c> <c es> <g c> <c es> <g c>] |
	<c,,2 c'> r4 |
	<as2 as'> r4 |
	<bes2 bes'> r4 |
	<es,2 es'> r4 |
%15
	bes'8 [<bes' f'> <f' as> <bes, f'> <f' as> <bes, f'>] |
	es,8 [<bes' es> <es g> <bes es> <es g> <bes es>] |
	<g,2 g'> r4 |
	<c2 c'> r4 |
	<as2 as'> r4 |
	<es2 es'> r4 |
	<bes'8 bes'> [<f'' bes> <bes d> <f bes> <bes d> <f bes>] |
	<es,,8 es'> [<es'' g bes> <g bes es> <es g bes> <g bes es> <es g bes>] |
	<g,,2 g'> r4 |
	<c2 c'> r4 |
	<f,2 f'> r4 |
	<c'2 c'> r4 |
	<g8 g'> [<d'' g> <g b> <d g> <g b> <d g>] |
	c,8 [<c' e g> <e g c> <c e g> <e g c> <c e g>] |
}

bassEentje = \notes\relative c{
	\property Voice.dynamicDirectionection=1
	<c,8 c'> [<c' f as!> <f as c> <c f as> <f as c> <c f as>] |
	c,8 [<c' e g> <e g c> <c e g> <e g c> <c e g>] |
	<g,8 g'> [<d'' g> <g b> <d g> <g b> <d g>] |
	c,8 [<e' g> <g c> <e g> <g c> <e g>] |
	<c,8 c'> [<c' f a> <f a c> <c f a> <f a c> <c f a>] |
	c,8 [<c' e g> <e g c> <c e g> <e g c> <c e g>] |
	<g,8 g'> [<d'' g> <g b> <d g> <g b> <d g>] |
	c,8 [<e' g> <g c> <e g> <g c> <e g>] |
}

bassThrough = \notes\relative c{
	\property Voice.dynamicDirectionection=1
	%61
	<g,8^"cresc." g'> [<g' b d> <b d f> <g b d> <as!-> b-> d->> <b d f>] |
	<g,8 g'> [<g' d'> <d' f> <g, d'> <as-> b-> d->> <b d f>] |
	% copied
	<g,8 g'> [<g' d'> <d' f> <g, d'> <as-> b-> d->> <b d f>] |
	<g,8 g'> [<g' d' e> <d' f> <g, d'> <gis-> b-> d->> <b d f>] |
	%65
	<gis,8 gis'> [<d''\> e> <e b'> <d e> <e b'> <d\! e>] |
	<a,8 a'> [<c' e> <e a> <c e> <e a> <c e>] |
	<a,8 a'> [<a' d f> <d f a> <a d f> <d f a> <a d f>] |
	<a,8 a'> [<a' c e> <c e a> <a c e> <c e a> <a c e>] |
	% 4 bars copied from end verse1
	<f,2\p f'> r4 |
	%70
	<c'2 c'> r4 |
	<g8 g'> [<d'' g> <g b> <d g> <g b> <d g>] |
	c,8\> [<c' e g> < \! e g c> <c e g> <e g c> <c e g>] |

	<c,8 c'> [<c' es! g> <es g c> <c es g> <es g c> <c es g>] |
	<f,,8 f'> [<d'' f> <f as!> <d f> <f as> <d f>] |
	%75
	<g,,8 g'> [<d'' f> <f g> <d f> <f g> <d f>] |
	c,8 [<c' e> <e g> <c e> <e g> <c e>] |
	c,8 [<c' f> <f as> <c f> <f as> <c f>] |
	c,8 [<c' e> <e g> <c e> <e g> <c e>] |
	<g,8 g'> [<g' d'> <d' f> <g, d'> <d' f> <g, d'>] |
	%80
	c,8 [<c' e> <e g> <c e> <e g> <c e>] |
	c,8 [<c' g> <e c> <c g> <e c> <c g>] |
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

allLyrics = \lyrics {
	\lyricVerseOne
	\lyricVerseTwo
	\lyricThrough
}

lyricStaff = \context Lyrics = lyric<
	\allLyrics
>
		
vocals = \notes{
	\clef treble;
 	% certainly no auto-beaming for vocals
 	\property Voice.noAutoBeaming = "1"
	\property Staff.automaticMelismata=1

	\property Voice.dynamicDirectionection = \up
	\skip 4 * 12; 
	\vocalVerse 
	\skip 4 * 24; 
	\vocalVerse
	\vocalThrough
}

vocalStaff = \context Staff = vocal<
	  \property Staff.instrument = "synth voice"
	  \global
	  \vocals
>

treble = {
	\clef treble;
	\property Voice.beamAutoBegin=0
	\trebleIntro 
	\trebleVerseOne 
	\trebleEentje
	\trebleVerseOne 
	\trebleThrough
}

trebleStaff = \context Staff = treble< 
	\global
	\treble
>

bass = {
	\clef bass;
	\bassIntro 
	\bassVerseOne 
	\bassEentje
	\bassVerseOne 
	\bassThrough
}

bassStaff = \context Staff = bass<
	\global
	\bass
>

grandStaff = \context PianoStaff <
	\trebleStaff
	\bassStaff
>


\score{
	<

%{
	Transpose as you like for your voice
	Range untransposed is c' to f'' (for tenors and sopranos)
	\transpose a gives a' to d'' (for basses, who sing an octave down)

		\addlyrics 
			\notes \transpose a \vocalStaff
			\lyricStaff
		\notes \transpose a \grandStaff
%}

		\addlyrics
			%\context Staff=vocal \vocalStaff
			%\context Lyrics=lyric \lyricStaff 
			\vocalStaff
			\lyricStaff 
		\grandStaff
	>
	\paper { 
		\translator { \HaraKiriStaffContext }
	}
	\midi{
		\tempo 4 = 54;
	}
}
