\header {
  filename =  "pa.ly";
  title =    "Wachet auf, ruft uns die Stimme";
  opus =    "BWV";
  composer =  "Johann Sebastian Bach 1685-1750 ";
  enteredby =  "JCN";
  copyright =  "public domain";
}

%% ?
#(define Span_bar_engraver-visibility #f)
#(define Staff_group_bar_engraver-visibility #f)
#(define Span_score_bar_engraver-visability #f)


% these should be two separate scores...
\score{
  \context Score <
   \context StaffGroup<
   \context PianoStaff <
    \context Staff = treble {
     \property Score.midiInstrument = "church organ"
     <
     \context Voice=i \notes\relative c { c c c c }
     >
    }
    \context Staff = bass \notes\relative c { c c c c }
   > 
   \context Staff = pedal \notes\relative c { c c c c }
   >
  >
\paper {
  textheight = 280.0 \mm;
%{
  \translator { 
    \OrchestralScoreContext 
    minVerticalAlign = 4.0*\staffheight;
    maxVerticalAlign = 4.0*\staffheight;
   }
%}
  \translator { 
    \StaffGroupContext
    minVerticalAlign = 4.0*\staffheight;
    maxVerticalAlign = 4.0*\staffheight;
    glyph = "brace";
   }
   \translator { 
    \PianoStaffContext
	minVerticalAlign = 2.5*\staffheight;
	maxVerticalAlign = 2.5*\staffheight;
   }
  }
  \midi {
   \tempo 4 = 69;
  }
}
