\score{
    \context PianoStaff < 
	 \context Staff=one \notes\relative c''{
	     fis,\arpeggio
	     %%\property PianoStaff.SpanArpeggio \push #'connect = ##t
	     \property PianoStaff.connectArpeggios = ##t
	     <fis,\arpeggio a c>
	  }
	 \context Staff=two \notes\relative c{
	     \clef bass;
	     g
	     <g\arpeggio b d>
	 }
    >
}
