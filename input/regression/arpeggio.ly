\version "1.5.68"
\header{
texidoc="
Arpeggios are supported, both cross-staff and broken single staff.
"
}



\score{
    \context PianoStaff < 
	 \context Staff=one \notes\relative c''{
	    \context Voice < fis,-\arpeggio  d a >
	    \property Staff.Arpeggio \override #'arpeggio-direction = #1 
	    \context Voice < fis,-\arpeggio  d a >	    
	     %%\property PianoStaff.SpanArpeggio \override #'connect = ##t
	     \property PianoStaff.connectArpeggios = ##t
	     <fis,\arpeggio a c>
	  }
	 \context Staff=two \notes\relative c{
	     \clef bass
	    \context Voice < g b d-\arpeggio   >
	    \property Staff.Arpeggio \override #'arpeggio-direction = #-1 	    
	    \context Voice < g b d-\arpeggio   >	    
	     <g\arpeggio b d>
	 }
    >
}
