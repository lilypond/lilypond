\header {
    title = "Sängers Morgenlied"
    composer = "Franz Schubert (1797-1828)"
    date = "27. Februar 1815"
    source = "Edition Peters"
    editor = "Paul Losse"
    enteredby="Han-Wen Nienhuys"
    maintainer="hanwen@xs4all.nl"

    texidoc = "The source is a rather tightly set Peters in Edition is
    a heavy font. The Peters edition (4622c) was `herausgegeben' by
    Paul Losse, whose name also appears on a 1956 edition of some
    other music. Strictly speaking, his edititorial enhancements will
    not be in the PD - but I am assuming there are no notable ones in
    this small piece.

    The original compresses the entire music onto a single page, in 4
    systems.  Lily does so too if you tune down spacing-increment, but
    chooses line breaks differently.

    Further manual tweaks: the slur in measure 12 has been flattened
manually. The beam in measure 3, left-hand, technically is wrong, but
has been added following the original. The crescendo in measure 4 has
been lowered

"

}

\version "2.21.0"
manuscriptBreak = { \break }



#(set-global-staff-size (/ (* 5.8 72.27) 25.4))

\paper  {
%#(set-global-staff-size (* 5.8 mm))
    line-width = #(* mm 160)
    indent = 8\mm
    system-system-spacing.basic-distance = #10.3
    ragged-bottom = ##t 
    }

modernAccidentals = {
  \set Staff.extraNatural =  ##f
  \set Staff.autoAccidentals =  #'(Staff (same-octave . 1) (any-octave . 0))
  \set Staff.autoCautionaries =  #'()  
}


melody =    \relative c'' \repeat volta 2 \new Voice = "singer" {
    \time 6/8
    \autoBeamOff
    << s2.^\markup { \bold \large\larger\larger \line { \hspace #-3.2 Lieblich, etwas geschwind } }
       R2.
    >>
  r4 r8 c4 g8 |
  e4 c8
    <<
	\new Voice { \stemUp f8. g16 }
	{ \stemDown f8.[ g16] } >> \stemNeutral a8 |
  fis4  g8 c16[ b a g] f[ e] |
  d4 f8 a16[ g fis g] f[ d] |
  g4. r8 gis gis |
  a4 a16.[ b32] c4 fis,8 |
  g4.~ 8-\fermata g8 g |
  as4 as8 g4 g8 |
  fis4 fis8 r8 g g  |
  a!4 a8 g4 g8 |
  fis4 fis8 d'16[ c b a] g[ f] |
  e4 g8 d4 g8 e4 r8 b'16[ c d b] g[ f] |
  e4 g8 d4 a'16[ g] |
  c,4 r8 r4 r8 |

  R2.^\fermata
}


ignoreMelisma =	\set ignoreMelismata = ##t
ignoreMelismaOff = \unset ignoreMelismata 


firstVerse = \lyricmode {
    \set stanza = "1."
    
    Sü -- ßes Licht! Aus
    \ignoreMelisma
    gol --
    \ignoreMelismaOff

    de -- nen  Pfor -- ten brichst du __ \manuscriptBreak | 
    sie -- gend durch __ die Nacht. Schö -- ner Tag, du __ bist er -- wacht. __ Mit ge -- |
    \manuscriptBreak
    heim -- nis -- vol -- len Wor -- ten, in me -- lo -- di -- schen Ak -- kor -- den, grüß __ ich __ \manuscriptBreak |
    dei -- ne Ro -- sen -- pracht, grüß __ ich __ dei -- ne Ro -- sen -- pracht. 
    }

secondVerse = \lyricmode {
    \set stanza = "2."
    Ach, der Lie -- be sanf
    -- tes We -- hen schwellt mir |
    das be -- weg -- te __ Herz, sanft, wie ein ge -- lieb -- ter Schmerz. __ Dürft ich | 
    nur auf gold -- nen Hö -- hen mich im Mor -- gen -- duft er -- ge -- hen! Sehn -- sucht |
    zieht mich him -- mel -- wärts, Sehn -- sucht zieht mich him -- mel -- wärts.
    }

pianoRH =  \relative c''' \repeat volta 2 {
    \accidentalStyle modern
    g16(_\p fis a g fis g f e d c b a ) | 
    <g e>8( <es fis a> <d f b> <c e c'>) r8 r | 
    r8 c'( e,) f r a |
    \once \override DynamicLineSpanner.padding =#3
    r8 << { s8\> s8\! }  << { fis( g)
			    } \\ { c,4 } >> >> r8 <e c g> <e c g> |
    <d c a>4. r8 \clef bass  <d b f> <d b f> |
    e,16_" "_\markup { \bold\italic cresc. } g c g e g d gis b gis d gis |
    c, e a e c e a,-\f d fis d a d |
    b d g  d b g r4\fermata \clef treble g''8 |
    as4.( g 4.) | fis4. r4 <d g>8 ( |
    <f a>4.) <e g>4.( <es fis> ) <d f>\sf |
    r8 <e c g> <e c g> r <d b g > <d b g> |
    r <e c g> <e c g> r <f d b g> <f d b g> |
    r <e c g> <e c g> r <d b f> <d b f> |
    c16(_\f b c e g b c b c e g <e b'>) |
    <c c'>8 r r <c, g e>8 r r\fermata |  
}

pianoLH =  \relative c'' \repeat volta 2 {
    \accidentalStyle modern
    g16( fis a g fis g f e d c b a) | 
    \clef bass g4.( c,8) r r
    \clef treble r4 <bes' c>8([ <a c>)] r <f c'> |
    \clef bass r8 dis( e) r c c |
    f,4. r8 g g |
    <c, c,>4. <e e,>4. |
    a,4. <d d,>4. |
    g,8 r r g16 r16\fermata r8 g''8 |
    as4.( g ) |
    fis r4 <g b>8( |
    <f c'>4.)

    \once \override Slur.height-limit = #1.0

    <g c>4.( | <a c>4.) <g b,> |
    c,4 r8 g4 r8 |
    c4 r8 g4 r8 |
    c4 r8 g4 r8 |
    <c g e c>8 <c e g> <c e g>     <c e g> <c e g> <c e g> |
    <c e g> r r <c, c,>8 r r\fermata \clef treble
    }

\book {
    \score {
	<< \time 6/8
	   \new Staff <<
	       \context Staff \accidentalStyle modern
	       \melody >>
	   \new Lyrics \lyricsto "singer" \firstVerse
	   \new Lyrics \lyricsto "singer" \secondVerse
	   \new PianoStaff << 
	       \set PianoStaff.instrumentName = \markup {
		   \bold
		   \larger\larger\larger\larger \huge "2.  " }
	       \new Staff \pianoRH
	       \new Staff \pianoLH
	   >> 
       >>

	\layout {
	    \context {
		\Lyrics
		\override LyricText.font-size = #-1
	    }
	    \context {
		\Score
		\override Beam.beam-thickness = #0.55
		\override SpacingSpanner.spacing-increment = #1.0
		\override Slur.height-limit = #1.5
	    }
	}
	\midi {
	  \tempo 4 = 70
	}
    }
}

