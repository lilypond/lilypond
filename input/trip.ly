\header {
  title =       "Trip test"
  subtitle = "Make life very difficult for lilypond"
  copyright =   "public domain"
  
  footer ="This file tests as much features of lily as possible."
}


\version "1.5.68"

%{

This file tests as much features of lily as possible. If there are any
basic faults in lily, this file will not run correctly.

TODO:

* autochange.

* tremolo

* lyrics.

* drum, jazz, pop notation

%}


praeludiumRight =   \notes {
   \key e \major
  \clef violin

  % 13 -- how to type -- where to split -- this more neatly?
  \context Staff <
    \context Voice = I \relative c'' { \stemUp r4 dis4 e4. e8 ~ |
      \shiftOn e4-.-^^\f [d8 fis8] \shiftOff gis4 ~ [gis8 fis16 e ] |
      fis4 ~ [fis8 e16 dis] e4 r8 e8 }
    \context Voice = III \relative c'' { \stemUp \shiftOn r4 bis cis \shiftOff cis |
      a' ~ [a16 gis a b] \shiftOn dis,4 cis ~ |
      [cis8 dis16 ais] bis4 cis r8 b }
    \context Voice = IV \relative c'' {
      \stemDown
      \shiftOn s4 gis }
      
    \context Voice =  II \relative c' { \stemDown
%      \shiftOn       % idem

      r4 fis \shiftOff gis gis |
      a4. cis8 gis2 |
      fis4 gis gis r8 e8 }
  > |
  % 16
}

praeludiumLeft =  \notes \relative c {
   \key e \major
  \clef bass

  % 13
  \context Staff <
    \context Voice = two { r4 }
    \context Voice = one { \stemUp s4 dis' cis cis ~ |
    \times 4/5
      {  [cis8 ( cis

      a \translator Staff = treble  \stemDown \shiftOn d
         )

      cis] }
      \translator Staff = bass 
      \shiftOff \stemUp
      [bis gis] cis4 |
      dis2 cis4 r8 cis }
    \context Voice = one { \stemUp bis2 }
    \context Voice = three {
    \property Voice.DynamicLineSpanner \override #'direction  = #-1
    \stemUp \shiftOn r4 gis ~ [gis8
      \<

     gis] ~ \stemDown \shiftOff gis4 |
     a4. fis8
       \!
     gis4. a8 ~ |
     a4 gis4 gis r8 gis }
    \context Voice = four { \stemDown \shiftOn s4 fis4 e}
    \context Voice = two { \stemDown s4 dis4 cis4 }
  > |
  %16
}



    
fugaIIRight =  \notes   \relative c''   {
  \key e \major              % E-major
  \clef violin
  \time 3/4

  %15
  \context Staff <
     { [b8 fis8] b4 }\\
     { fis2 }
  >

  \context Staff \notes\relative c''<
   {
	  e4 -5
       }\\
       \\
 {
	  cis-4
       }\\ \\
       
        {
	  ais-3 
       }\\ \\ 
       {
	  fis-2
       }
  >

    %16
    \context Staff <
 {  dis2 dis4 |
      cis2 cis4 |
      b4. [cis8 dis e] } \\
 {   fis2. ~ |
      fis ~ |
      fis4 e2 } \\
 {   [b8 fis] b2 ~ |
      [b8 a!16 gis] a2 ~ |
      a4 gis2 }
  > |
  %19
}

gracetest =  \notes \grace { [c16 ( cis dis] }

fugaIILeft =  \notes {
  \key e \major
  \clef bass

  %15
  \context Staff < 
     { 
    \gracetest
    \relative b, < )b2 dis fis a b cis dis> \stemUp ais4 |
      b2 b4 } \\
      {  s2 e4 | fis2 fis4 }
  >
  \stemBoth
  cis2 [e16( fis a \clef "treble" b] |
  d'4 ) b8 b8 b4 |
  %19
}

fugaIIPedal =  \notes \relative c {
  \key e \major
  \clef bass

  \repeat "volta" 2 { dis4.-\ltoe } \alternative { e8-\rtoe cis4 } |
  b4.-\lheel [cis8-\ltoe dis8-\rtoe e8-\rheel] |
  fis4.-\rtoe [e8-\rheel dis8-\rtoe cis8-\ltoe] |
  dis4-\rtoe e4-\rheel e,4-\ltoe |
  %19
}

% these should be two separate scores...
\score{
  \context Score  \notes <
    \context PianoStaff <
      \context Staff = treble {
	\property Staff.instrument = #"right"
	\property Staff.instr = #"rt"
	\property PianoStaff.instrument = #"hands"
	\property PianoStaff.instr = #"hs"


 	\property Score.midiInstrument = "church organ"
        \praeludiumRight 
	 \times 4/3 {  c4 c4 c4 }  \mark "B"
	   \fugaIIRight }
      \context Staff = bass {
	\property Staff.instrument = #"left"
	\property Staff.instr = #"lt"
        \praeludiumLeft r1 \fugaIILeft }
    > 
    \context Staff = pedal \relative c  <
      {
        \property Staff.instrument = #"bass"
        \property Staff.instr = #"bs"	

        \time 4/4
	\key e \major 
  \clef bass

  %13
    \property Staff.PhrasingSlur \override #'dashed = #5.0
    c4 \( ( d ) e\) f

%    r4 fis,4-\ltoe e4.-\lheel e'8-\rheel | 

% tie accs: 2nd should get no acc
f4-\rtoe~ f8
fis8-\rtoe fis4-\rtoe [e8-\ltoe a-\rtoe] | 
  dis,4-\ltoe gis-\rtoe [cis,8-\ltoe
    (
     b!-\lheel ais-\rtoe g-\ltoe ~ ] |

% tie should generate acc.
\break
      g  r4.
    )
      c2^^^-^\f 
 \time 3/4

  \fugaIIPedal \bar "|."  }
      
    >
  >

  \paper {

	\translator {
		\OrchestralScoreContext
	}
	\translator { \PianoStaffContext
		\consists "Instrument_name_engraver"
	}
	\translator { \StaffContext
		\consists "Instrument_name_engraver"
	}
  }

  \midi {
    \tempo 4 =96 }
}
