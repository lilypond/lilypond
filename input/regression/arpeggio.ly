\version "1.7.6"
\header{
texidoc="
Arpeggios are supported, both cross-staff and broken single staff.
"
}



\score{
    \context PianoStaff < 
	 \context Staff=one \notes\relative c''{
	    \context Voice << fis,  d a >>-\arpeggio
	    \property Staff.Arpeggio \override #'arpeggio-direction = #1 
	    \context Voice << fis,  d a >>-\arpeggio	    
	     %%\property PianoStaff.SpanArpeggio \override #'connect = ##t
	     \property PianoStaff.connectArpeggios = ##t
	     <<fis, a c>>-\arpeggio
	  }
	 \context Staff=two \notes\relative c{
	     \clef bass
	    \context Voice << g b d   >>-\arpeggio
	    \property Staff.Arpeggio \override #'arpeggio-direction = #-1 	    
	    \context Voice << g b d   >>-\arpeggio	    
	     <<g b d>>-\arpeggio
	 }
    >
}
