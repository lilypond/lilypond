%% why non of this any effect?
#(define Span_bar_engraver-visibility #f)
#(define Staff_group_bar_engraver-visibility #f)
#(define Span_score_bar_engraver-visability #f)

#(assoc-set! bar-break-glyph-alist "bracket" '(nil . nil))

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
  \translator { 
    \StaffGroupContext
    minVerticalAlign = 4.0*\staffheight;
    maxVerticalAlign = 4.0*\staffheight;
% this doesn't work
    barType = #"|"
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
