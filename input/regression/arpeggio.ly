
\version "2.1.7"
\header{
texidoc="
Arpeggios are supported, both cross-staff and broken single staff.
"
}



\score{
    \context PianoStaff\notes << 
	 \new Staff \relative c''{
	     <fis,  d a>\arpeggio
	    \property Staff.Arpeggio \override #'arpeggio-direction = #1 
	     <fis d a >\arpeggio	    
	     %%\property PianoStaff.SpanArpeggio \override #'connect = ##t
	     \property PianoStaff.connectArpeggios = ##t
	     <fis d a>\arpeggio
	  }
	 \new Staff\relative c{
	     \clef bass
	     <g b d>\arpeggio
	    \property Staff.Arpeggio \override #'arpeggio-direction = #-1
	     <g b d>\arpeggio
	     <g b d>\arpeggio
	 }
    >>
    \paper { raggedright= ##t }
}
