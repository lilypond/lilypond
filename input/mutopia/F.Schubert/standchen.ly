#(ly:set-option 'old-relative)

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
  mutopiapublicdomain = "\\parbox[b]{\\paper-width}{\\thefooter\\quad\\small
    \\\\This music is part of the Mutopia project,
    \\texttt{http://www.mutopiaproject.org/}\\\\It has been typeset
    and placed in the public domain by " + \maintainer +
    ".\\\\Unrestricted modification and redistribution is permitted
    and encouraged---copy this music and share it.}"
 tagline = \mutopiapublicdomain
 footer = "Mutopia-2001/04/27-xx"
} 

\version "2.11.61"

#(set-global-staff-size 16)

vocalVerse = \relative c''{
	\dynamicUp
	\times 2/3 {  g8[(  as)] g } c4. g8 |
	\times 2/3 {  f8[(  g)] f } c'4 f,8 r |
	g4.-> f8 \times 2/3 {  f[(  es)] d } |
	es2 r4 |
	R2. |
	R2. |
	\times 2/3 {  g8[(  as)] g } es'4. g,8 |
	\times 2/3 {  f8[(  g)] f } d'4. c8 |
	bes4. as8 \times 2/3 {  as[(  g)] f } |
	g2 r4 |
	R2. |
	R2. |
	g8. b16 es4. d8 |
	c8. g16 es4. c8 |
	\grace {
  \override Stem   #'stroke-style = #"grace"
    as'32[( bes ] 
  \revert Stem #'stroke-style }
 \times 2/3 {  as8[)(  g)] as } c4. as8 |
	g2. |
	\grace {
  \override Stem   #'stroke-style = #"grace"
    f32[( g ] 
  \revert Stem #'stroke-style }
 \times 2/3 {  f8[)(  e)] f } as4. f8 |
  
	es!2. |
	g8. b16 es4. d8 |
	c8. g16 e4. c8 |
 	\grace {
  \override Stem   #'stroke-style = #"grace"
    a'32[( b ] 
  \revert Stem #'stroke-style }
 \times 2/3 {  a!8[)(  gis)] a } c4. a8 |
	g!2. |
	\times 2/3 {  d'8[\f(  cis)] d } f4. b,8 |
	c!2. |
}

vocalThrough = \relative c{
	\dynamicUp
	g''8. g16 b8. b16 d8. d16 |
	c4 b r |
	g4. b8 d8. c16 |
	b2 r4 |
	e4. d8 \times 2/3 {  d[(  c)] b } |
	a8. b16 c4-> a8 r |
	R2. |
	R2. |
	\grace {
  \override Stem   #'stroke-style = #"grace"
    a32[( b ] 
  \revert Stem #'stroke-style }
 \times 2/3 {  a!8[)(  gis)] a } c4. a8 |
	g!2. |
	\times 2/3 {  d'8[\f(  cis)] d } f4. b,8 |
	c!2. ~ |
	% bug: slur on other staff fools lily into extending melisma
	c4 r c |
	as2. |
	g |
	e2 r4 |
}

lyricVerseOne = \lyricmode {
	Lei -- se fle -- hen mei -- ne Lie -- der
	durch die Nacht zu dir;
	in den stil -- len Hain her nie -- der,
	Lieb -- chen, komm zu mir! 
	
	Flüs -- ternd schlan -- ke Wip -- fel rau -- schen
	in des Mon -- des Licht,
	in des Mon -- des Licht;

	des Ver -- rä -- ters feind -- lich Lau -- schen
	fürch -- te, Hol -- de, nicht,
	fürch -- te, Hol -- de, nicht.
}
	
lyricVerseTwo = \lyricmode{
	Hörst die Nach -- ti -- gal -- len schla -- gen?
	ach! sie fle -- hen dich, 
	mit der Tö -- ne sü -- "\ss en" Kla -- gen
	fle -- hen sie für mich. 
	
	Sie -- ver -- "steh'n" des Bu -- sens Seh -- nen,
	ken -- nen Lieb -- es -- schmerz,
	ken -- nen Lieb -- es -- schmerz,

	rüh -- ren mit den Sil -- ber -- tö -- nen
	jed -- es wei -- che Herz,
	jed -- es wei -- che Herz.
}

lyricThrough = \lyricmode{
	La\ss auch dir die Brust be -- we -- gen 
	Lieb -- chen, hö -- re mich! 
	be -- bend harr' ich dir ent -- ge -- gen! 
	
	komm, be -- glük -- ke mich!
	komm, be -- glük -- ke mich, __ 
	be -- glük -- ke mich!
}

trebleIntro = \relative c{
	r8^"\bf Mäßig"\pp <g' c>-. <c es>-. <g c>-. <c es>-. <g c>-. |
	r8 <as c>-. <c es>-. <as c>-. <c es>-. <as c>-. |
	r8 <as c>-. <c d>-. <as c>-. <c d>-. <as c>-. |
	r8 <g b>-. <b d>-. <g b>-. <b d>-. <g b>-. |
}

trebleVerseOne = \relative c{
	%5
	r8 <g' c> <c es> <g c> <c es> <g c> |
	r8 <f c'> <c' d> <f, c'> <c' d> <f, c'> |
	r8 <f g b> <g b d> <f g b> <g b d> <f g b> |
	r8 <es g c> <g c es> <es g c> <g c es> <es g c> |
	<g'' b,>4.( <f d>8)
	\times 2/3 { <f d>([ <es c> <d b>)] }

	%10
	<c es>2. |
	r8 <g, c> <c es> <g c> <c es> <g c> |
	r8 <f c'> <c' d> <f, c'> <c' d> <f, c'> |
	r8 <f as bes> <as bes d> <f as bes> <as bes d> <f as bes> |
	r8
	<es g bes>[
	<g bes es>
	<es g bes>
	<g bes es>]
	<es' c'>(
	<d bes'>4.()
	<f as>8)
	
	\times 2/3 { <f as>([ <es g> <d f>)] }
	%16
	<es g>2. |
	r8 <f, g> <g b> <f g> <g b> <f g> |
	r8 <es g> <g c> <es g> <g c> <es g> |
	r8\pp <es as c> <as c es> <es as c> <as c es> <es as c> |
	%20
	r8 <es g bes> <g bes es> <es g bes> <g bes es> <es g bes> |
	\grace {
  \override Stem   #'stroke-style = #"grace"
    as'32[( bes ] 
  \revert Stem #'stroke-style }

	\times 2/3 {  as8[)( g as] } c4.->  as8) |
	g2. |
	r8 <f, g> <g b> <f g> <g b> <f g> |
	r8 <e g> <g c> <e g> <g c> <e g> |
	r8 <f a c> <a c f> <f a c> <a c f> <f a c> |
	r8 <e g c> <g c e> <e g c> <g c e> <e g c> |
	\times 2/3  { <f' f'>\f( <e e'> <f f'> }
	<a a'>4.-> <f f'>8)  |
}

trebleEentje =  \relative c' \context Voice {
	\stemNeutral
	<e e'>2 r4 |
	<f as!>2\(\mf <as c>8.(-> <f as>16)\) |
	% urg: slurs with staccati are ugly
	<e g>4. <e g>8-.( <e g>-. <e g>)-. |
	<f g>4. <b, g'>8-.( <d g>-. <f g>)-. |
	<e g>2 <e g>4\pp |
	<f a>2\( <a c>8.( <f a>16)\) |
	<e g>4.\( <e g>8(-. <e g>-. <e g>)\)-. |
	<f g>4. <b, g'>8(-. <d g>-. <f g>)-. |
	%60
	<e g>2 r4 |
}

trebleThrough =  \relative c'{
	\stemNeutral
	<e e'>2. |
	%61
	R2. |
	<g, g'>8.[\< <g g'>16 <b b'>8. <b b'>16\>\! <d d'>8. <d d'>16] |

	%% score has double slur on chord.
	<c c'>4( <b c'>) r\!

	<g g'>4. <b b'>8 <d' d, >8.->[ c16] |
	%65
        <d, e b'>2.~(\f
	<c e a>2)  r4
	<< 
	{ 	a'8. b16 c4^> ( a8) r |
		a8. b16 c4^> ( a8) r |
	}\\{ 
		<d, f>4 <d f>2 |
		<c! es>4 <c es>2 |
	} >>

	% 4 bars copied from end verse1
	r8 <f, a c> <a c f> <f a c> <a c f> <f a c> |
	%70
	r8 <e g c> <g c e> <e g c> <g c e> <e g c> |
	\times 2/3 {  <f' f'>8( <e e'> <f f'> }

	<a a'>4.-> <f f'>8)
	<e e'>2. |
	<es! es'! >2. |
	\override TextScript   #'font-shape = #'italic
	<d d'>2._"decresc."
	\revert TextScript #'font-shape

	%75
	<b b'>2. |
	<c c'>2 <e g>4\pp |

	% four copied from begin eentje
	<f as!>2( <as c>8.-> <f as>16) |
	<e g>4. <e g>8(-. <e g>-. <e g>)-. |
	<f g>4. <b, g'>8(-. <d g>-. <f g>)-. |
	%80

	\override TextScript   #'font-shape = #'italic
	<e g>2._"dim."
	\revert TextScript #'font-shape

	<g, e' g>2. |
	<g e' g>2.\fermata |
}

bassIntro = \relative c{
	\dynamicUp
%1
	<c, c'>2 r4 |
	<as as'>2 r4 |
	<f f'>2 r4 |
	<g g'>2 r4 |
}

bassVerseOne = \relative c{
%	\clef bass
	\dynamicUp
%5
	<c, c'>2 r4 |
	<as as'>2 r4 |
	<g g'>2 r4 |
	<c c'>2 r4 |
	<g g'>8 <g'' d'>[ <d' f> <g, d'> <d' f> <g, d'>] |
%10
	<c,, c'>8 <g'' c>[ <c es> <g c> <c es> <g c>] |
	<c,, c'>2 r4 |
	<as as'>2 r4 |
	<bes bes'>2 r4 |
	<es, es'>2 r4 |
%15
	bes'8 <bes' f'>[ <f' as> <bes, f'> <f' as> <bes, f'>] |
	es,8 <bes' es>[ <es g> <bes es> <es g> <bes es>] |
	<g, g'>2 r4 |
	<c c'>2 r4 |
	<as as'>2 r4 |
	<es es'>2 r4 |
	<bes' bes'>8 <f'' bes>[ <bes d> <f bes> <bes d> <f bes>] |
	<es,, es'>8 <es'' g bes>[ <g bes es> <es g bes> <g bes es> <es g bes>] |
	<g,, g'>2 r4 |
	<c c'>2 r4 |
	<f, f'>2 r4 |
	<c' c'>2 r4 |
	<g g'>8 <d'' g>[ <g b> <d g> <g b> <d g>] |
	c,8 <c' e g>[ <e g c> <c e g> <e g c> <c e g>] |
}

bassEentje = \relative c{
	\dynamicUp
	<c, c'>8 <c' f as!>[ <f as c> <c f as> <f as c> <c f as>] |
	c,8 <c' e g>[ <e g c> <c e g> <e g c> <c e g>] |
	<g, g'>8 <d'' g>[ <g b> <d g> <g b> <d g>] |
	c,8 <e' g>[ <g c> <e g> <g c> <e g>] |
	<c, c'>8 <c' f a>[ <f a c> <c f a> <f a c> <c f a>] |
	c,8 <c' e g>[ <e g c> <c e g> <e g c> <c e g>] |
	<g, g'>8 <d'' g>[ <g b> <d g> <g b> <d g>] |
	c,8 <e' g>[ <g c> <e g> <g c> <e g>] |
}

bassThrough = \relative c{
	\dynamicUp
	%61
	<g, g'>8^"cresc." <g' b d>[ <b d f> <g b d> <as! b d >-> <b d f>] |
	<g, g'>8 <g' d'>[ <d' f> <g, d'> <as b d >-> <b d f>] |
	% copied
	<g, g'>8 <g' d'>[ <d' f> <g, d'> <as b d >-> <b d f>] |
	<g, g'>8 <g' d'>[ <d' f> <g, d'> <gis b d >-> <b d f>] |
	%65
	<gis, gis'>8 <d'' e>[\> <e b'> <d e> <e b'> <d e> \!] |
	<a, a'>8 <c' e>[ <e a> <c e> <e a> <c e>] |
	<a, a'>8 <a' d f>[ <d f a> <a d f> <d f a> <a d f>] |
	<a, a'>8 <a' c e>[ <c e a> <a c e> <c e a> <a c e>] |
	% 4 bars copied from end verse1
	<f, f'>2\p r4 |
	%70
	<c' c'>2 r4 |
	<g g'>8 <d'' g>[ <g b> <d g> <g b> <d g>] |
	c,8\> <c' e g>[ < e g c> \! <c e g> <e g c> <c e g>] |

	<c, c'>8 <c' es! g>[ <es g c> <c es g> <es g c> <c es g>] |
	<f,, f'>8 <d'' f>[ <f as!> <d f> <f as> <d f>] |
	%75
	<g,, g'>8 <d'' f>[ <f g> <d f> <f g> <d f>] |
	c,8 <c' e>[ <e g> <c e> <e g> <c e>] |
	c,8 <c' f>[ <f as> <c f> <f as> <c f>] |
	c,8 <c' e>[ <e g> <c e> <e g> <c e>] |
	g,8 <g' d'>[ <d' f> <g, d'> <d' f> <g, d'>] |
	%80
	c,8 <c' e>[ <e g> <c e> <e g> <c e>] |
	c,8 <c' g>[ <e c> <c g> <e c> <c g>] |
	<c, g' c>2._\fermata |
}
		
global = {
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

allLyrics = {
	% maybe should be bigger by default, in grob-description.scm ?
	\lyricsto "leise" \new Lyrics {
	    \lyricVerseOne
	    \lyricVerseTwo
	    \lyricThrough 
} }

vocals = \new Voice =  "leise"  {
	\clef treble
 	% certainly no auto-beaming for vocals
 	\autoBeamOff

	\dynamicUp
	% duh 1 != 3/4
	R1 * 3/4 * 4
	\vocalVerse 
	R1 * 3/4 * 8
	\vocalVerse
	\vocalThrough
	R1 * 3/4 * 6
}

trebleStaff = \new Staff =  "treble"<< 
        \set Staff.midiInstrument = "acoustic grand"
	\global
	{
	  \clef treble

	  \trebleIntro 
	  \trebleVerseOne 
	  \trebleEentje
	  \trebleVerseOne 
	  \trebleThrough
	}
>>

bassStaff = \new Staff =  "bass"<<
        \set Staff.midiInstrument = "acoustic grand"
	\global
	\clef bass
	{\bassIntro 
	\bassVerseOne 
	\bassEentje
	\bassVerseOne 
	\bassThrough}
>>


\score{
    <<
	\new Staff <<
	    \set Staff.midiInstrument = "synth voice"
	    %% insert \transpose if necessary, depending on voice range.
	    \global
	    \vocals
	>>
	\allLyrics
	\context PianoStaff <<
	    \trebleStaff
	    \bassStaff
	>>
    >>
    \layout {
				% Use
				%   textheight = 280.\mm
				%   line-width = 190.\mm
				% to get this on 3 pages of a4.
	
				% Mandatory Mutopia settings yield 4 pages :(
	textheight = 270.0\mm
	line-width = 180.0\mm

	\context { \RemoveEmptyStaffContext }
    }
    
  \midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 54 4)
      }
    }


}

