\version "1.3.146"
\header {
  filename =  "pa.ly"
  title =    "Wachet auf, ruft uns die Stimme"
  opus =    "BWV"
  composer =  "Johann Sebastian Bach 1685-1750 "
  enteredby =  "JCN"
  copyright =  "public domain"
}



commands = \notes {
  \time 4/4
  \property Staff.TimeSignature \override #'style = #'C
  \key es \major
  \partial 8
}

right = \notes \relative c' {
  \commands 
%if urtekst
  \clef "alto"
%  \clef "violin"

%if stable
%  \property Voice."beamAutoEnd_8" = "1/4"
  \property Voice.autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 1 4)
  bes8 |
  es16 f g8 g f as g bes, as |
  \repeat "volta" 2 {
  g'16 es f8 as, g d' es r bes |
  es16 f g8 g f as g bes, as|
  g'16 es f8 as, g d' es r bes' |
  bes4 as16 g f es f es d c bes8 c16 d |
  es f g f  as g f es g8 f r bes, |
  g' a4 bes8 \grace f8()es16 d es8 r c |
  a'8 bes4 c8 \grace f,()es16 d es8 r \grace{\slurDown [c'16( d ]}\stemBoth)es8 |
  d16 c bes8 bes16\prall a bes8 ~bes16 a g f es d c bes|
  c d es8 \grace f()es16 d es8~es16 a bes c bes a g f |
  bes8 f d\trill c16 bes bes es d c d8 bes|
  \grace a()g8. a16 a8.\prallprall g32 a bes4 r8 bes|
  es16 f g8 g f as g bes, as|
  g'16es f8 as, g d' es r bes|
  es16 f g8 g f as g bes, as|
  g'16 es f8 as , g d' es r f |
  g a4 bes8 \grace f()es16 d es8 r es|
  a bes4 c8 \grace g()f16 es f8 r4 |
  r r8 bes bes4 as16 g f es|
  f es d c bes8 c16 d es f g f as g f es
  }
  \alternative { 
  { g8 f r4 r r8 bes,| es16 f g8 g f as g bes, as }
  { g8 f r4 r r8 bes | } }
  bes'4 as16 g f es f es d c bes8 c16 d |
  es f g f as g f es g8 f r bes, |
  g' a4 bes8 \grace {f(}) es16 d es8 r c |
  a' bes4 c8 \grace {f,(})es16 d es8 r es' |
  d16 c bes8 bes16\prall a bes8~bes16 a g f es d c bes|
  c d es8 es16\prall d es8~es16 a bes c bes a g f|
  bes8 f d\trill c16 bes bes es d c d8 bes|
  g8. a16 a8.\prallprall g32 a bes4 r8 d|
  es f4 g8 bes,16\prall as bes8 r bes'|
  bes4 as16 g f es f es d c bes8 c16 d|
  es f g f as g f es g8 f r4|
  r r8 g c16 d es8 es d |
  f es g, f es'16 c d8 f, es|
  b' c r g g4 f16 es d c |
  d c b a g8 a16 b c d es d f es d c|
  es8 d r g, es'fis4 g8|
  \grace d()c16 bes c8 r a fis' g4 a8|
  \grace d,() c16 bes c8 r c' bes16 a g8~g16\trill fis g8~|
  g16 f es d c bes a g a bes c8~c16\trill bes c8~|
  c16 fis g a g fis e d g8 d bes\trill a16 g |
  g4 r r r8 bes|
  es16 f g8 g f as g bes, as|
  g'16 es f8 as, g d' es r4|
  r r8 bes' bes4 as16 g f es|
  f es d c bes8 c16 d es f g f as g f es |
  g8 f r es c' d4 es8|
  as,16\prall g as8 r c d es4 f8|
  \grace bes,() as16 g as8 r f g16 f es8~es16\trill d es8~|
  es16 des' c bes as g f es f g as8~as16\trill g as8~|
  as16 d, es f es d c bes bes'8 es, g16 f es d |
  es as g f g8 es \grace d()c8. d16 d8. es16|
  es1|
  \bar "|."
}

clefs = \notes{
  \clef "alto"
  \partial 8
  \skip 8
%testin'
%  \clef "violin"
  \skip 1
%  \clef "alto"
  \repeat "volta" 2 { \skip 1*19 }
  \alternative {
  { \skip 1*2 }
  { \skip 1*1 } }
  \skip 1*11
  s4 \clef "violin"
  s4 s2
  s1
  s4 s8 \clef "alto" s8 s2
  \skip 1*12
  s4 r8 \clef "violin" s8 s2 \clef "alto"
}

left = \notes \relative c {
  \commands 
%if urtekst
  \clef "tenor"
%  \clef "bass"

%if stable
%  \property Staff.noVoltaBraces = 1 
%  \property Voice."beamAutoEnd_8" = "1/4"
  \property Voice.autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 1 4)
%  \property Staff.noVoltaBraces = ##t
  \property Staff.VoltaBracket = \turnOff
  r8 
  R1
  \repeat "volta" 2 {
  R1*11
  r2 es4 g |
  bes bes bes bes|
  c2 bes |
  r r4 bes|
  es bes es8 f g4|
  f4. es8 d4 c8.\prallprall bes32 c|
  bes2 r|
  r4 bes \grace bes8() es4 bes|
  }
  \alternative {
  { c g8. as32 bes as8 g f4\trill| es2 r | }
  { c4 g8. as32 bes as8 g f4\trill |} }
  es,2 r |
  R1*6 |
  r2 r4 bes'4|
  bes as g f\trill |
  es2 r|
  r4 bes' bes as|
  g f\trill es2|
  R1|
  f4 g as2\trill|
  g r|
  R1*5|
  r4 bes c d|
  es2 r|
  r r4 es8 f|
  g4 f\trill es2|
  R1|
  r4 bes es bes|
  c g as8 g f4\trill|
  es1|
  R1*4
  \bar "|."
}

pedal = \notes \relative c, {
  \commands 
%if stable
%  \property Staff.noVoltaBraces = 1
%  \property Staff.noVoltaBraces = ##t
  \property Staff.VoltaBracket = \turnOff
  \clef "bass"
  r8 |
  es4 es es g
  \repeat "volta" 2 {
  as bes es, r|
  es es es g | 
  as bes es, r | 
  g c bes as |
  g es bes' d |
  es d c es|
  f g a f|
  bes a g f |
  es d c es|
  d8 bes f' f, g a bes d,|
  es c f4 bes8 c bes as|
  g f es d c4 c'|
  g bes es, g |
  as as' d, es |
  c d es d|
  c g c8 d es4~|
  es8 d g, a bes d, es f|
  bes a bes c d bes es4|
  bes as! g es |}
  \alternative {
  { as8 bes c4 f, bes8 as| g4 es' d es |}
  { as8 bes c4 f, bes8 as}}
  g4 c bes as |
  g es bes' d|
  es d c es|
  f g a f|
  bes a g f|
  es  d c es|
  d8 bes f' f, g a bes d,|
  es c f4 bes8 c bes as!|
  g f es d es g as bes|
  c d es4 bes as |
  g es bes'8 c d bes |
  es d c b c g' c bes!|
  as4 es f g|
  as8 g f e f es d c|
  b g g' f es4 c|
  g' b c bes|
  a c, d es|
  fis, d g f|
  es' d c bes|
  a c bes8 g d' d,|
  g es f g as g as bes|
  c bes c d es4 c|
  as bes c8 bes c d |
  es d es f g f g as|
  d, bes bes'as g4 es|
  bes'8 as g4 as g|
  f es f bes, |
  c as bes g|
  as8 g f es d4 f|
  bes8 c bes as g c as bes|
  c d es g as f bes bes,|
  es1
  \bar "|."
}




% these should be two separate scores...
\score{
  \context Score <
   \context StaffGroup<
   \context PianoStaff <
    \context Staff = treble {
     \property Score.midiInstrument = "church organ"
     <
%if urtekst
     \context Voice=i \clefs
     \context Voice=ii \right
     >
    }
    \context Staff = bass \left
   > 
   \context Staff = pedal \pedal
   >
  >

\paper {
  %textheight = 280.0 \mm
  % landscape:
%  textheight = 160.0 \mm
  orientation = "landscape"
  linewidth = 280.0 \mm
%{
  \translator { 
    \OrchestralScoreContext 
    minVerticalAlign = 4.0*\staffheight
    maxVerticalAlign = 4.0*\staffheight
   }
%}
  \translator { 
    \StaffGroupContext
    minVerticalAlign = 4.0*\staffheight
    maxVerticalAlign = 4.0*\staffheight
   }
   \translator { 
    \PianoStaffContext
	minVerticalAlign = 2.5*\staffheight
	maxVerticalAlign = 2.5*\staffheight
   }
  }
  \midi {
   \tempo 4 = 69
  }
}
