\header {
    title = "Sängers Morgenlied"
    composer  = "Franz Schubert"
    date = "27. Februar 1815"
    source = "Edition Peters"
    editor = "Paul Losse"
    enteredby="Rune Zedeler"
    maintainer="rune@zedeler.dk"

    texidoc = "The source is a rather tightly set Peters in Edition is
    a heavy font. The Peters edition (4622c) was `herausgegeben' by
    Paul Losse, whose name also appears on a 1956 edition of some
    other music. Strictly speaking, his edititorial enhancements will
    not be in the PD - but I am assuming there are no notable ones in
    this small piece.

    The original compresses the entire music onto a single page, in 4
    systems.  Lily does so too if you tune down spacing-increment, but
    chooses line breaks differently.

"

}

\version "2.1.3"
manuscriptBreak = { \break }
\paper  {
    staffheight = 6.0 \mm
    
    #(define fonts (scale-font-list (/ (* 6 mm) (* 20 pt)) ))
    
    \include "params-init.ly"
    linewidth = #(* mm 160)
    indent = 8\mm
    }

modernAccidentals = {
  \property Staff.extraNatural = ##f
  \property Staff.autoAccidentals = #'(Staff (same-octave . 0) (any-octave . 0) (same-octave . 3))
  \property Staff.autoCautionaries = #'()  
}


melody = \notes   \relative c'' \repeat volta 2 \context Voice = singer {
    \time 6/8
    \autoBeamOff
    s1*0^\markup { \bold \bigger\bigger { \hspace #-3.0 Lieblich, etwas geschwind } }
  R2.
  r4 r8 c4 g8 |
  e4 c8 << { f8. g16 } \\ { f8.[ g16] } >> a8 |
  fis4  g8 c16[ b a g] f[ e] |
  d4 f8 a16[ g fis g] f[ d] |
  g4. r8 gis gis |
  a4 a16.[ b32] c4 fis,8 |
  g4.~ g8-\fermata g8 g |
  as4 as8 g4 g8 |
  fis4 fis8 r8 g g  |
  a4 a8 g4 g8 |
  fis4 fis8 d'16[ c b a] g[ f] |
  e4 g8 d4 g8 e4 r8 b'16[ c d b] g[ f] |
  e4 g8 d4 a'16[ g] |
  c,4 r8 r4 r8 |
  R2.\fermata 
}


firstVerse = \lyrics {
    \property LyricsVoice . stanza = "1."
    
    Sü -- ßes Licht! Aus gol -- de -- nen  Pfor -- ten brichst du __ \manuscriptBreak
    sie -- gend durch __ die Nacht. Schö -- ner Tag, du __ bist er -- wacht. __ Mit \manuscriptBreak
    ge -- heim -- nis -- vol -- len Wor -- ten, in me -- lo -- di -- schen Ak -- kor -- den, grüß __ ich __ \manuscriptBreak
    dei -- ne Ro -- sen -- pracht, grüß ich __ dei -- ne Ro -- sen -- pracht. 
    }

secondVerse = \lyrics {
    \property LyricsVoice . stanza = "2."

    Ach, der Lie -- be sanf "" -- tes We -- hen schwellt mir
    das be -- weg -- te __ Herz, sanft, wie ein ge -- lieb -- ter Schmerz. __ Dürft ich
    nur auf gold -- nen Hö -- hen mich im Mor -- gen -- duft er -- ge -- hen! Sehn -- sucht
    zieht mich him -- mel -- wärts, Sehn -- sucht zieht mich him -- mel -- wärts
    }

pianoRH = \notes \relative c''' \repeat volta 2 {
    g16(_\p fis a g fis g f e d c b a ) | 
    <g e>8( <es fis a> <d f b> <c e c'>) r8 r | 
    r8 c'( e,) f r a |
    \once \property Voice.DynamicLineSpanner \set #'padding =#3
    r8_\> << { s8 s8-\! }  << { fis( g)
			    } \\ { c,4 } >> >> r8 <e c g> <e c g> |
    <d c a>4. r8 \clef bass  <d b f> <d b f> |
    e,16_" "_\markup { \italic cresc } g c g e g d gis b gis d gis |
    c, e a e c e a,-\f d fis d a d |
    b d g  d b g r4\fermata \clef treble g''8 |
    as4.( g 4.) | fis4. r4 <d g>8 ( |
    <f a>4.) <e g>4.( <es fis> ) <d f>\sf |
    r8 <e c g> <e c g> r <d b g > <d b g> |
    r <e c g> <e c g> r <f d b g> <f d b g> |
    r <e c g> <e c g> r <d b f> <d b f> |
    c16( b c e g b c b c e g <e b'>) |
    <c c'>8 r r <c, g e>8 r r\fermata |  
}

pianoLH = \notes \relative c'' \repeat volta 2 {
    
    g16( fis a g fis g f e d c b a) | 
    \clef bass g4.( c,8) r r
    \clef treble r4 <bes' c>8( <a c>) r <f c'> |
    \clef bass r8 dis( e) r c c |
    f,4. r8 g g |
    <c, c,>4. <e e,>4. |
    a,4. <d d,>4. |
    g,8 r r g16 r16\fermata r8 g''8 |
    as4.( g ) |
    fis r4 <g b>8( |
    <f c'>4.) <g c>4.( | <a c>4.) <g b,> |
    c,4 r8 g4 r8 |
    c4 r8 g4 r8 |
    c4 r8 g4 r8 |
    <c g e c>8 <c e g> <c e g>     <c e g> <c e g> <c e g> |
    <c e g> r r <c, c,>8 r r\fermata \clef treble
    }

\score {

    << \time 6/8
	\addlyrics
     \new Staff {
	 \context Staff \modernAccidentals
	 \melody }
     \new Lyrics <<
	 \context  LyricsVoice = "singer-1" \firstVerse
	 \context LyricsVoice = "singer-2" \secondVerse
	 >>
     \new PianoStaff << 
	 \property PianoStaff.instrument = \markup {
	     \bold
	     \huge "2.  " }
	 \new Staff \pianoRH
	 \new Staff \pianoLH
	>> 
    >>

    \paper {
	\translator {
	    \LyricsVoiceContext
	    minimumVerticalExtent = ##f
	    LyricText \set #'font-size = #1.0
	}
	\translator {
	    \ScoreContext
	    Beam \override #'thickness = #0.6
	    SpacingSpanner \set #'spacing-increment = #1.0
	}
	\translator {  \PianoStaffContext
		VerticalAlignment \override #'forced-distance = #10
		}
	\translator {
	     \ScoreContext
	     \remove "Lyric_phrasing_engraver"
	     \consists "New_phrasing_engraver"
	 }

}
}
