\header {
filename = "utremi.ly";
enteredby = "Christian Mondrup";
%composer = "Thomas Ravenscroft";
opus = "Thomas Ravenscroft";
%opus = "Pammelia, 1609, no. 31";
arranger = "Pammelia, 1609, no. 31";
title = "Vt, re, me, fa, sol, la, sol, fa, re, vt";
tagline = "Typeset with GNU LilyPond by Christian Mondrup. Non-commercial copying welcome.";
}

papersize = "letter"

\include "paper16.ly"
%\include "a4.ly"

lyricsOne = \lyrics {
   ""\breve ""8
   "Vt,"\breve re, me, fa, sol, la, ""
   sol, la, fa, me, re, vt. 
}

lyricsTwo = \lyrics {
   "" ""
   Hey downe downe hey downe downe downe downe hey down
   hey down "" down down a.
   My heart of gold as true as steele
   as I me leant "" vn -- to the bowres.
   but if my La -- dy loue me well,
   Lord so Ro -- bin bowres,
}

lyricsThree = \lyrics {
   "" "" "" "" "" "" "" "" "" "" "" ""
   heaue and hoe Rum -- be -- lo, hey tro -- lo tro -- ly lo,
   hey tro -- ly trol -- ly hey tro -- ly trol -- ly 
   hey tro -- ly trol -- ly hey trol -- "" ly trol -- ly lo. 
   My La -- dies gone to Can -- ter -- bu -- ry
   S. Tho -- mas be her boote.
   Shee met with Kate of Malms -- "" bu -- ry,  
   why weepst thou ma -- ple 
}

lyricsFour = \lyrics {
   "" 
   roote:
   O sleepst thou or wakst thon Ies -- se -- ry, Cooke,
   the rost it burnes, turne round turne round "" a -- bout ""
   turne round "" a -- bout, ""
   turne round "" a -- bout, "" turne round. ""
   O Fri -- er how fares thy ban -- de -- low ban -- de -- low
   Fri -- er, how fares thy San -- de -- low, San -- de -- low.
}

global = \notes {
   \key c;
   %\time 4/2;
   %\time 17/8;
   \time 33/16;
   \clef "tenor";
}

dummyBeat = \notes {
   \property Staff.defaultBarType = "||"
   \property Staff.barAlways = "1" 
   s16
   \property Staff.barAlways = "0"
   \property Staff.defaultBarType = "|"
}

incipOne = \notes\relative c' {
   g\breve 
   \dummyBeat
   \bar "";
}

incipTwo = \notes\relative c'' {
   \property Voice.noteHeadStyle = "diamond"
   g1. 
   \property Voice.noteHeadStyle = ""
   f2 
   \dummyBeat
   \bar "";
}

incipThree = \notes\relative c' {
   \property Voice.noteHeadStyle = "harmonic"
   \property Voice.tupletVisibility = 0
   \times 2/3 { d4. e8 d4 } \times 2/3 { b4. a8 g4 }
   \times 2/3 { b4. c8 b4 } \times 2/3 { b4. a8 g4 }
   \dummyBeat
   \property Voice.tupletVisibility = 3
   \bar "";
}

incipFour = \notes\relative c' {
   \property Voice.noteHeadStyle = "diamond"
   %\property Voice.restStyle = "mensural"
   b1 r1
   \dummyBeat
   \bar "";
}

partOne = \notes\relative c' {
   %\property Score.currentBarNumber = "1"
   \property Voice.noteHeadStyle = ""
   %\property Staff.barNumberScriptPadding = 1
   g1 ~ | g | a ~ | a | bes ~ | bes | c ~ | c | d ~ | d | e ~ | e |
   r | r | 
   e ~ | e | d ~ | d | c ~ | c | bes ~ | bes | a ~ | a | g ~ | g
   \bar "|.";
}

partTwo = \notes\relative c'' {
   \property Voice.noteHeadStyle = ""
   g2. f4 | e2 d | c4. b8 a4 g | f2 e |
   d g ~ | g4 a g2 | c,1 | r2 c |
   g'2. a4 | b2 g | c c | c c |
   b4 c d2 ~ | d4 d b g | c1 | r2 c |
   g g | d2. e4 | f2 e | c1 |
   g'2. g4 | g2 g | c,1 ~ | c | r | r 
   \bar "|.";
}

partThree = \notes\relative c' {
   \property Voice.noteHeadStyle = ""
   \times 2/3 { d4. e8 d4 } \times 2/3 {b4. a8 g4 } | 
   \times 2/3 { b4. c8 b4 } \times 2/3 { b4. a8 g4 }
   c2 c4 c | c2 c |
   d2 d4 d | d2 d | e2. f4 | e2 d4 c |
   b2 g ~ | g4 f e d | c1 | r2 c |
   g'2. g4 | g2 g | c,2. c4 | c c c'2 |
   bes g | bes bes | a1 | r2 e' |
   d d | g g | a2. g4 | fis e d2 |
   g g | d2. c4 
   \bar "|.";
}

partFour = \notes\relative c' {
   \property Voice.noteHeadStyle = ""
   b1^\fermata | r2 g' | e e4 e | a2. g4 | 
   fis2 e4 d | g1 | g2 g | g g |
   g g | g g ~ | g4 f e d | c2 d ~ |
   d4 c b a | g2 g' ~ | g4 f e d | c2 g' ~ |
   g r | \times 2/3 { r1 d2 } \times 2/3 { a'2 a g } |
   \times 2/3 { e1 f2 } \times 2/3 { g2. g4 g2 }
   \times 2/3 { d2. d4 e2 } \times 2/3 { f2. f4 e2 }
   \times 2/3 { d1 c2 } \times 2/3 { b2. a4 g2 }
   \times 2/3 { d'2. d4 d2 }
   \bar "|.";
}

partOneStaff =  <
   \context Staff = vocal1 <
      \property Staff.clefStyle = "fullSizeChanges"
      \property Staff.instrument = "\large{1.}"
      %\property Staff.instr = ""
      \notes { 
	 \global
         \property Staff.timeSignatureStyle = "old4/4"
         \incipOne
         \clef "G_8";
	 \property Staff.timeSignatureStyle = "C2/2"
	 \time 4/4;
	 \partOne 
      }
      {\context Lyrics = lyrOne \lyricsOne }
   >
>

partTwoStaff = <
   \context Staff = vocal2 <
      \property Staff.clefStyle = "fullSizeChanges"
      \property Staff.instrument = "\large{2.}"
      %\property Staff.instr = ""
      %\property Voice.automaticMelismata = "1"
      \addlyrics
      \notes { 
	 \global
         \property Staff.timeSignatureStyle = "old4/4"
         \incipTwo
	 \clef "G_8";
	 \property Staff.timeSignatureStyle = "C2/2"
	 \time 4/4;
	 \partTwo
      }
      {\context Lyrics = lyrTwo \lyricsTwo }
   >
>


partThreeStaff = <
   \context Staff = vocal3 <
      \property Staff.clefStyle = "fullSizeChanges"
      \property Staff.instrument = "\large{3.}"
      %\property Staff.instr = ""
      %\property Voice.automaticMelismata = "1"
      \addlyrics
      \notes { 
         \context Voice = vthree 
	    \property Voice.tupletDirection = \up
	 \global
         \property Staff.timeSignatureStyle = "old4/4"
         \incipThree
	 \clef "G_8";
	 \property Staff.timeSignatureStyle = "C2/2"
	 \time 4/4;
	 \partThree
      }
      {\context Lyrics = lyrThree \lyricsThree }
   >
>

partFourStaff = <
   \context Staff = vocal4 <
      \property Staff.clefStyle = "fullSizeChanges"
      \property Staff.timeSignatureStyle = "C2/2"
      \property Staff.barNumberDirection = \up
      \property Staff.instrument = "\large{4.}"
      \property Staff.instr = ""
      %\property Voice.automaticMelismata = "1"
      \addlyrics
      \notes { 
         \context Voice = vfour 
	    \property Voice.tupletDirection = \up
	 \global
	 %\property Staff.instrument = "\large{4.}"
	 %\property Staff.instr = ""
         \property Staff.timeSignatureStyle = "old4/4"
         \incipFour
	 \clef "G_8";
	 \property Staff.timeSignatureStyle = "C2/2"
	 \time 4/4;
	 \partFour 
      }
      {\context Lyrics = lyrFour \lyricsFour} 
   >
>

\score {
   \context StaffGroup <
      \partFourStaff
      \partThreeStaff
      \partTwoStaff
      \partOneStaff
  >
   %\paper {\translator {\BarNumberingStaffContext } }	
   \paper {
      indent = 0.\mm;
      linewidth = 17.0\cm;
      textheight = 27.0\cm;
      gourlay_maxmeasures=6.0;
      \translator { \StaffContext
         \consists "Staff_margin_engraver"; }	
      %\translator { \ScoreContext
      %   minVerticalAlign = 1.5*\staffheight; }
   }
   %\midi { 
   %   output = "utremi.mid";
   %   \tempo 2 = 80;
   %}
}
