
hairyChord = \context Staff \notes\relative c' <
    \context Voice=one {
        \property Voice.Stem \push #'direction = #1
	 \property Voice.NoteColumn \push #'horizontal-shift = #0
	 e4\arpeggio
    }
    
    \context Voice=two {
	 \property Voice.Stem \push #'direction = #1
	 \property Voice.NoteColumn \push #'horizontal-shift = #1
	 cis\arpeggio
	 }
    
    \context Voice=three {
    	\property Voice.Stem \push #'direction = #1
	\property Voice.NoteColumn \push #'horizontal-shift = #2
	ais\arpeggio
	}
    
    \context Voice=four {
	\property Voice.Stem \push #'direction = #-1
	\property Voice.NoteColumn \push #'horizontal-shift = #-1
	fis\arpeggio
	}
>


\score{
    \notes \transpose c'' {
        \context Voice < fis''-\arpeggio g  d a >
        \context Voice < fis,-\arpeggio g  d a >
        \context Voice < fis''-\arpeggio g  d a >
        \hairyChord
	}
    \paper {
        linewidth = -1.;
	\translator{
	    \StaffContext
	    connectArpeggios = ##t
	}
	}
}
