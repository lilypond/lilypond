\version "1.3.146"
\header{
texidoc="
Arpeggios are supported, both cross-staff and broken single staff.
Directed arpeggios are indicated by arrows, while a square bracket 
to the left prevents arpeggiation. 
"
}



\score {
     \context PianoStaff < 
	 \context Staff=one \notes\relative c'{
	    \context Voice {
	     < a4 d fis-\arpeggio a >
	     \property Voice.Arpeggio \override #'arpeggio-type = #'bracket
	     < d fis-\arpeggio a d >
     %%\property PianoStaff.SpanArpeggio \override #'connect = ##t
	     \property PianoStaff.connectArpeggios = ##t
	     <b-\arpeggio d g b>
 	     \property PianoStaff.Arpeggio \override #'arpeggio-type = #'bracket
 	     <c-\arpeggio e g c>
 	     \property PianoStaff.connectArpeggios = ##f
 	     \property Voice.Arpeggio \override #'arpeggio-type = #'down
 	     <a1-\arpeggio d fis a>

	    }
	  }
	 \context Staff=two \notes\relative c {
	     \clef bass
	     \context Voice {
	     r4 r 
	     <d,\arpeggio a' d >
 	     <d \arpeggio a' d >
 	     \property Voice.Arpeggio \override #'arpeggio-type = #'up
 	     <g1 \arpeggio b d g>
	    }
	  }
    >
}
