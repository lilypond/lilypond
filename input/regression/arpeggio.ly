
\version "2.3.22"
\header{
texidoc="
Arpeggios are supported, both cross-staff and broken single staff.
"
}



\score{
    \context PianoStaff << 
	 \new Staff \relative c''{
	     <fis,  d a>\arpeggio
	    \override Staff.Arpeggio  #'arpeggio-direction = #1 
	     <fis d a >\arpeggio	    
	     %%\override PianoStaff.SpanArpeggio  #'connect = ##t
	     \set PianoStaff.connectArpeggios = ##t
	     <fis d a>\arpeggio
	  }
	 \new Staff\relative c{
	     \clef bass
	     <g b d>\arpeggio
	    \override Staff.Arpeggio  #'arpeggio-direction = #-1
	     <g b d>\arpeggio
	     <g b d>\arpeggio
	 }
    >>
    \layout { raggedright= ##t }
}
