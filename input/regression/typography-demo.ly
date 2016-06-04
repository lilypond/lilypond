\header {
  title = "LilyPond demo"
  enteredby="Han-Wen Nienhuys"
  maintainer="hanwen@xs4all.nl"

  texidoc = "
heavily mutilated Edition Peters Morgenlied by Schubert"

}

\version "2.19.21"

ignoreMelisma =	\set ignoreMelismata = ##t
ignoreMelismaOff = \unset ignoreMelismata 

#(set-global-staff-size 21)

\paper  {
				%#(set-global-staff-size (* 5.8 mm))
  indent = #(* mm 4)
  line-width = #(* mm 140)
  system-system-spacing.basic-distance = #10.3
  ragged-bottom = ##t 

  % Font settings for Cyrillic and Hebrew
  % Linux Libertine fonts contain Cyrillic and Hebrew glyphs.
  #(define fonts
    (set-global-fonts
     #:roman "Linux Libertine O,serif"
     #:sans "Linux Biolinum O,sans-serif"
     #:typewriter "Linux Libertine Mono O,monospace"
   ))
}

modernAccidentals = {
  \set Staff.extraNatural =  ##f
  \set Staff.autoAccidentals =  #'(Staff (same-octave . 1) (any-octave . 0))
  \set Staff.autoCautionaries =  #'()  
}


melody =    \relative c'' \repeat volta 2 \context Voice = "singer" {
  \time 6/8
  \autoBeamOff
  << s2.^\markup {  \larger \line { \hspace #-3.1 Lieblich, etwas geschwind } }
     R2.
  >>
  r4 r8 c4 g8 |
  \acciaccatura { f16 }  e4 c8
  <<
    \new Voice { \stemUp f8. g16 }
    { \stemDown f8.[ g16] } >> \stemNeutral a8 |
  fis4  g8 c16[ b a g] f[ e] |
  d4 f8
  \transpose a' e' \relative { a'16[ g fis! g] f![ d]  } |
  g4. r8 gis gis |
  a4 a16.[ b32] c8[( a]) fis8 |
  g4.~ 8-\fermata
}


firstVerse = \lyricmode {
  \set stanza = "1."
  
  Sü -- ßes Licht! Aus
  \ignoreMelisma
  gol --
  \ignoreMelismaOff

  de -- nen  Pfor -- ten brichst du __ | 
  sie -- gend durch __ die Nacht. Schö -- ner Tag, du __ bist er -- wacht. __ 
}

secondVerse = \lyricmode {
  \set stanza = "2."
  いろはに כיף та та ほへど ちり  ぬるを
  
  Жъл  дю ля זה
  
  いろ はに כיף та та ほへ ちり ぬる
  
  Жъл дю ля __

}

pianoRH =  \relative c''' \repeat volta 2\new Voice {
  \accidentalStyle modern
  \voiceOne
  g16( fis a g fis g f e d c b
  \oneVoice
  a ) | 
  <g e>8( <es fis a> <d e bes'> <c e c'>\arpeggio) r8 r | 
  r8 c'( e,) f r a |
  \once \override DynamicLineSpanner.padding =#3
  r8
  << { fis( g) } \\
     << { a4 } { s8\> s8\! } >>
   >>

  r8 <e c g>8[  <e c g>] |
  <d c a>4. r8 \clef bass  <d b f> <d b f> |
		\crescTextCresc
		e,16_" "\<
		g c g e g d gis b gis d gis |
		c, e a e c e a,-\f\! d fis d a d |
		b d g  d b g e16. r32\fermata
	      }

pianoLH =  \relative c'' \repeat volta 2\new Voice {
    \accidentalStyle modern
    \voiceTwo
    g16( fis a g fis g

    f e d c b

    \change Staff = down
	\oneVoice
    d,) | 
     g4.( b,8) r r
    \clef treble \grace s16 r8 <bes'>8-> <bes c>8->([ <a c>)] r <f c'> |
    \clef bass
    r8 dis( e) r c c |
    f,4.  g8[ r8 g] |
    <c, c,>4. <e e,>4. |
    a,4. <d d,>4. |
    g,8 r r g16 r16\fermata 
    }

  \book {
    \score {
      << \time 6/8
	 \new Staff \with {
	   fontSize = #-3
	   \override StaffSymbol.staff-space = #(magstep -3)
	 } <<
	   \context Staff \accidentalStyle modern
	   \melody >>
	 \new Lyrics \lyricsto "singer" \firstVerse
	 \new Lyrics \lyricsto "singer" \secondVerse
	 \new PianoStaff << 
	   \set PianoStaff.instrumentName = \markup {
	     \bold
	     \larger\larger\larger\larger
	     \huge
	     "2."
	   }
	   \context Staff = up <<
	     \pianoRH
	     \pianoLH
	   >>
	   \context Staff = down { \clef bass \skip 1*2 }
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
	  \override Beam.auto-knee-gap = #4.0
	  \override SpacingSpanner.spacing-increment = #1.0
	  \override Stem.stemlet-length = #0.5
	  \override Slur.height-limit = #1.5
	}
      }
      
  \midi {
    \tempo 4 = 70
    }


    }
  }

