\score{
    \context PianoStaff < 
	 \context Staff=one \notes\relative c''{
	    \context Voice < fis,-\arpeggio  d a >
	     %%\property PianoStaff.SpanArpeggio \push #'connect = ##t
	     \property PianoStaff.connectArpeggios = ##t
	     <fis,\arpeggio a c>
	  }
	 \context Staff=two \notes\relative c{
	     \clef bass;
	    \context Voice < g b d-\arpeggio   >
	     <g\arpeggio b d>
	 }
    >
}
