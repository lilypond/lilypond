
% #(set! point-and-click line-column-location)

instrument = "Piano"

\header {
  title = "Ständchen"
  subtitle = "(Serenade)"
  subsubtitle = "Leise flehen meine Lieder"
  opus = "D. 957 No. 4"
  date = "August 1828"
  composer = "Franz Schubert (1797-1828)"
  poet = "Text by Ludwig Rellstab (1799-1860)"
  enteredby = "JCN"
  copyright = "public domain"
  % instrument = \instrument

  % mutopia headers.
  mutopiatitle = "Standchen"
  mutopiasubtitle = "Leise flehen meine Lieder"
  mutopiacomposer = "Franz Schubert (1797-1828)"
  mutopiapoet = "Ludwig Rellstab (1799-1860)"
  mutopiaopus = "D957.4"
  mutopiainstrument = \instrument
  date = "1828/08"
  style = "Romantic"
  source = "Schubert-Lieder Edition Schott No. 608, (Not dated).
    Jubilaeums-Ausgabe zum 100. Todestage (~1928)."

  copyright = "Public Domain"
  maintainer = "Jan Nieuwenhuizen"
  maintainerEmail = "janneke@gnu.org"
  lastupdated =	 "2001/Apr/27"
  mutopiapublicdomain = "\\parbox[b]{\\hsize}{\\thefooter\\quad\\small
    \\\\This music is part of the Mutopia project,
    \\texttt{http://www.mutopiaproject.org/}\\\\It has been typeset
    and placed in the public domain by " + \maintainer +
    ".\\\\Unrestricted modification and redistribution is permitted
    and encouraged---copy this music and share it.}"
 tagline = \mutopiapublicdomain
 footer = "Mutopia-2001/04/27-xx"
} 

\version "1.5.68"

dynamicUp = \property Voice.DynamicLineSpanner \override #'direction = #1
dynamicRevert = \property Voice.DynamicLineSpanner \revert #'direction

\include "paper16.ly"

vocalVerse =  \notes\relative c''{
	\dynamicUp
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
	\grace { [ as'32( bes ] } \times 2/3 { [ )as8( )g] as } c4. as8 |
	g2. |
	\grace { [ f32( g ] } \times 2/3 { [ )f8( )e] f } as4. f8 |
  
	es!2. |
	g8. b16 es4. d8 |
	c8. g16 e4. c8 |
 	\grace { [ a'32( b ] } \times 2/3 { [ )a!8( ) gis] a } c4. a8 |
	g!2. |
	\times 2/3 { [ d'8\f( )cis] d } f4. b,8 |
	c!2. |
}

vocalThrough =  \notes\relative c{
	\dynamicUp
	g''8. g16 b8. b16 d8. d16 |
	c4 b r |
	g4. b8 d8. c16 |
	b2 r4 |
	e4. d8 \times 2/3 { [ d( )c] b } |
	a8. b16 c4-> a8 r |
	R2. |
	R2. |
	\grace { [ a32( b ] } \times 2/3 { [ )a!8( ) gis] a } c4. a8 |
	g!2. |
	\times 2/3 { [ d'8\f( )cis] d } f4. b,8 |
	c!2. ~ |
	% bug: slur on other staff fools lily into extending melisma
	c4 r c |
	as2. |
	g |
	e2 r4 |
}

lyricVerseOne =  \lyrics{
	Lei -- se fle -- hen mei -- ne Lie -- der
	durch die Nacht zu dir;
	in den stil -- len Hain her nie -- der,
	Lieb -- chen, komm zu mir! 
	
	Fl\"us -- ternd schlan -- ke Wip -- fel rau -- schen
	in des Mon -- des Licht,
	in des Mon -- des Licht;

	des Ver -- r\"a -- ters feind -- lich Lau -- schen
	f\"urch -- te, Hol -- de, nicht,
	f\"urch -- te, Hol -- de, nicht.
}
	
lyricVerseTwo =  \lyrics{
	H\"orst die Nach -- ti -- gal -- len schla -- gen?
	ach! sie fle -- hen dich, 
	mit der T\"o -- ne s\"u -- "\ss en" Kla -- gen
	fle -- hen sie f\"ur mich. 
	
	Sie -- ver -- "steh'n" des Bu -- sens Seh -- nen,
	ken -- nen Lieb -- es -- schmerz,
	ken -- nen Lieb -- es -- schmerz,

	r\"uh -- ren mit den Sil -- ber -- t\"o -- nen
	jed -- es wei -- che Herz,
	jed -- es wei -- che Herz.
}

lyricThrough =  \lyrics{
	La\ss auch dir die Brust be -- we -- gen 
	Lieb -- chen, h\"o -- re mich! 
	be -- bend harr' ich dir ent -- ge -- gen! 
	
	komm, be -- gl\"uk -- ke mich!
	komm, be -- gl\"uk -- ke mich, __ 
	be -- gl\"uk -- ke mich!
}

trebleIntro =  \notes\relative c{
	r8^"\bf M\\\"a\\ss ig"\pp <g'-. c-.> <c-. es-.> <g-. c-.> <c-. es-.> <g-. c-.> |
	r8 <as-. c-.> <c-. es-.> <as-. c-.> <c-. es-.> <as-. c-.> |
	r8 <as-. c-.> <c-. d-.> <as-. c-.> <c-. d-.> <as-. c-.> |
	r8 <g-. b-.> <b-. d-.> <g-. b-.> <b-. d-.> <g-. b-.> |
}

trebleVerseOne =  \notes\relative c{
	%5
	r8 <g' c> <c es> <g c> <c es> <g c> |
	r8 <f c'> <c' d> <f, c'> <c' d> <f, c'> |
	r8 <f g b> <g b d> <f g b> <g b d> <f g b> |
	r8 <es g c> <g c es> <es g c> <g c es> <es g c> |
	<g''4.( b,> <)f8 d>
	% manual beam override bug
	%\times 2/3 { < [f( d> <es c> <)d b] > } |
	\times 2/3 < { [ f( es )d ] } { d c b } > |
	%10
	<c2. es> |
	r8 <g, c> <c es> <g c> <c es> <g c> |
	r8 <f c'> <c' d> <f, c'> <c' d> <f, c'> |
	r8 <f as bes> <as bes d> <f as bes> <as bes d> <f as bes> |
	% manual beam override bug
	% r8 < [ es g bes> <g bes es> <es g bes> <g bes es ] > 
	r8 < { [ es g es g ] } { g bes g bes } { bes es bes es } >
	<{ es'( )  d4.() f8}{ c' | bes4.  as8 } >
	% manual beam override bug
	% \times 2/3 { < [f( as> <es g> <)d f] > } |
	\times 2/3 < { [ f( es )d ] } { as g f } > |
	%16
	<es2. g> |
	r8 <f, g> <g b> <f g> <g b> <f g> |
	r8 <es g> <g c> <es g> <g c> <es g> |
	r8\pp <es as c> <as c es> <es as c> <as c es> <es as c> |
	%20
	r8 <es g bes> <g bes es> <es g bes> <g bes es> <es g bes> |
	\property Voice.Slur \override #'attachment = #'(stem . stem)
	\grace { [ as'32( bes ] } \times 2/3 { [ )as8( g as] } c4.-> ) as8 |
	\property Voice.Slur \revert #'attachment
	g2. |
	r8 <f, g> <g b> <f g> <g b> <f g> |
	r8 <e g> <g c> <e g> <g c> <e g> |
	r8 <f a c> <a c f> <f a c> <a c f> <f a c> |
	r8 <e g c> <g c e> <e g c> <g c e> <e g c> |
	\times 2/3 < { [ f'8\f( e f] } {  f' e f } >
	< {a4.- > )f8}  { a'4. f8 }  > |
}

trebleEentje =  \notes \relative c'{
	\stemBoth
	<e2 e'> r4 |
	<f2\mf as!\(> <as8.->( c> <f16 \))as> |
	% urg: slurs with staccati are ugly
	<e4. g> <e8-. g-.(> <e-. g-.> <e-. )g-.> |
	<f4. g> <b,8-. g'-.(> <d-. g-.> <f-. )g-.> |
	<e2 g> <e4\pp g> |
	<f2 a\(> <a8.( c> <f16 \))a> |
	<e4.\( g> <e8-.( g-.> <e-. g-.> <e-. \))g-.> |
	<f4. g> <b,8-. g'-.(> <d-. g-.> <f-. )g-.> |
	%60
	<e2 g> r4 |
}

trebleThrough =  \notes \relative c'{
	\stemBoth
	<e2. e'> |
	%61
	R2. |
	[<g,8.\< g'> <g16 g'> <b8. b'> <\! b16\> b'16> <d8. d'> <d16 d'>] |
	< { c4( )b } { c'4( )b } > \!r |

	<g4. g'> <b8 b'> [<d'8.-> d,-> > c16] |
	%65
	< { d,2.(\f )a'2} { e2. ~ e2 } { b'2.( )c,2 }> r4 |
	\context Staff < 
		\context Voice=one {
			\voiceOne
			%urg
			%a8. b16 c4-> () a8 r |
			a8. b16 c4^> () a8 r |
			%a8. b16 c4-> () a8 r |
			a8. b16 c4^> () a8 r |
		}
		\context Voice=two {
			\voiceTwo
			<d,4 f> <d2 f> |
			<c!4 es> <c2 es> |
		}
	>

	% 4 bars copied from end verse1
	r8 <f, a c> <a c f> <f a c> <a c f> <f a c> |
	%70
	r8 <e g c> <g c e> <e g c> <g c e> <e g c> |
	\times 2/3 < { [ f'8\f( e f] } {  f' e f } >
	< { a4.-> )f8 } { a'4. f8 } > |
	<e2. e'> |
	<es!2. es'! > |
	\property Voice . TextScript \override #'font-shape = #'italic
	<d2._"decresc." d'> |
	\property Voice . TextScript \revert #'font-shape

	%75
	<b2. b'> |
	<c2 c'> <e4\pp g> |

	% four copied from begin eentje
	<f2 as!(> <as8.-> c> <f16 )as> |
	<e4. g> <e8-. g-.(> <e-. g-.> <e-. )g-.> |
	<f4. g> <b,8-. g'-.(> <d-. g-.> <f-. )g-.> |
	%80

	\property Voice . TextScript \override #'font-shape = #'italic
	<e2._"dim." g> |
	\property Voice . TextScript \revert #'font-shape

	<g,2. e' g> |
	<g2.-\fermata e' g> |
}

bassIntro =  \notes\relative c{
	\dynamicUp
%1
	<c,2 c'> r4 |
	<as2 as'> r4 |
	<f2 f'> r4 |
	<g2 g'> r4 |
}

bassVerseOne =  \notes\relative c{
%	\clef bass
	\dynamicUp
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

bassEentje =  \notes\relative c{
	\dynamicUp
	<c,8 c'> [<c' f as!> <f as c> <c f as> <f as c> <c f as>] |
	c,8 [<c' e g> <e g c> <c e g> <e g c> <c e g>] |
	<g,8 g'> [<d'' g> <g b> <d g> <g b> <d g>] |
	c,8 [<e' g> <g c> <e g> <g c> <e g>] |
	<c,8 c'> [<c' f a> <f a c> <c f a> <f a c> <c f a>] |
	c,8 [<c' e g> <e g c> <c e g> <e g c> <c e g>] |
	<g,8 g'> [<d'' g> <g b> <d g> <g b> <d g>] |
	c,8 [<e' g> <g c> <e g> <g c> <e g>] |
}

bassThrough =  \notes\relative c{
	\dynamicUp
	%61
	<g,8^"cresc." g'> [<g' b d> <b d f> <g b d> <as!-> b-> d->> <b d f>] |
	<g,8 g'> [<g' d'> <d' f> <g, d'> <as-> b-> d->> <b d f>] |
	% copied
	<g,8 g'> [<g' d'> <d' f> <g, d'> <as-> b-> d->> <b d f>] |
	<g,8 g'> [<g' d'> <d' f> <g, d'> <gis-> b-> d->> <b d f>] |
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
	g,8 [<g' d'> <d' f> <g, d'> <d' f> <g, d'>] |
	%80
	c,8 [<c' e> <e g> <c e> <e g> <c e>] |
	c,8 [<c' g> <e c> <c g> <e c> <c g>] |
	<c,2._\fermata g' c> |
}
		
global =  \notes{
	\time 3/4 
	\key es \major
	\skip 1 * 3/4 * 4
	\break
	\skip 1 * 3/4 * 25
	\break
	\skip 1 * 3/4 * 6
	\break
	\skip 1 * 3/4 * 41
	\break
	\skip 1 * 3/4 * 6
	\bar "|."
}

allLyrics = \lyrics {
	% maybe should be bigger by default, in grob-description.scm ?
	\property Lyrics . LyricText \override #'font-relative-size = #1
	\property Lyrics . LyricHyphen \override #'maximum-length = #1.5
	\lyricVerseOne
	\lyricVerseTwo
	\lyricThrough
}

lyricStaff = \context Lyrics {
	\allLyrics
}
		
vocals = \context Voice \notes {
	\clef treble
 	% certainly no auto-beaming for vocals
 	\property Voice.autoBeaming = ##f
	\property Staff.automaticMelismata= ##t

	\dynamicUp
	% duh 1 != 3/4
	R1 * 3/4 * 4
	\vocalVerse 
	R1 * 3/4 * 8
	\vocalVerse
	\vocalThrough
	R1 * 3/4 * 6
}

vocalStaff =  \context Staff = vocal<
	  \property Staff.midiInstrument = "synth voice"
	  \global
	  \vocals
>

treble =  {
	\clef treble
	\property Voice.autoBeamSettings \override #'(begin * * * *) = #(make-moment 0 1)
	\trebleIntro 
	\trebleVerseOne 
	\trebleEentje
	\trebleVerseOne 
	\trebleThrough
}

trebleStaff =  \context Staff = treble< 
        \property Staff.midiInstrument = "acoustic grand"
	\global
	\treble
>
bass =  {
	\clef bass
	\bassIntro 
	\bassVerseOne 
	\bassEentje
	\bassVerseOne 
	\bassThrough
}

bassStaff =  \context Staff = bass<
        \property Staff.midiInstrument = "acoustic grand"
	\global
	\bass
>

grandStaff =  \context PianoStaff <

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
			\vocalStaff
			\lyricStaff 
		\grandStaff
	>
	\paper {
		% Use
		%   textheight = 280.\mm
		%   linewidth = 190.\mm
		% to get this on 3 pages of a4.
		
		% Mandatory Mutopia settings yield 4 pages :-(
		textheight = 270.0\mm
		linewidth = 180.0\mm

		\translator { \HaraKiriStaffContext }
	}
	\midi{
		\tempo 4 = 54
	}
}
