#(ly:set-option 'old-relative)
\version "1.9.0"

\header  {
texidoc = "Arpeggio stays clear of accidentals and flipped note heads.
Since Arpeggio engraver is Voice, it does nothing for voice collisions."
}

hairyChord = \context Staff \notes\relative c' <
    \context Voice=one {
        \property Voice.Stem \override #'direction = #1
	 \property Voice.NoteColumn \override #'horizontal-shift = #0
	 e4-\arpeggio
    }
    
    \context Voice=two {
	 \property Voice.Stem \override #'direction = #1
	 \property Voice.NoteColumn \override #'horizontal-shift = #1
	 cis-\arpeggio
	 }
    
    \context Voice=three {
    	\property Voice.Stem \override #'direction = #1
	\property Voice.NoteColumn \override #'horizontal-shift = #2
	ais-\arpeggio
	}
    
    \context Voice=four {
	\property Voice.Stem \override #'direction = #-1
	\property Voice.NoteColumn \override #'horizontal-shift = #-1
	fis-\arpeggio
	}
>


\score{
    \notes \transpose c c' {
	<< fis'' g  d a >>-\arpeggio
	<< fis, g  d a >>-\arpeggio
	<< fis'' g  d a >>-\arpeggio
        \hairyChord
	}
    \paper {
        raggedright = ##t
	\translator{
	    \StaffContext
	    connectArpeggios = ##t
	}
	}
}


