
\score { 
  \context Voice \notes\relative c {
    \context Staff \notes\relative c''<
	     \context Voice=one {
		\property Voice.NoteColumn \override #'horizontal-shift = #0
		\stemUp\slurUp\tieUp 
		e4 
	     }
	     \context Voice=two {
		\stemUp\slurUp\tieUp 
		\property Voice.NoteColumn \override #'horizontal-shift = #1
		cis
	     }
	     \context Voice=three {
		\property Voice.NoteColumn \override #'horizontal-shift = #2
		\stemUp\slurUp\tieUp 
		ais
	     }
	     \context Voice=four {
		\stemDown\slurDown\tieDown 
		\property Voice.horizontalNoteShift=-1
		fis
	     }
	>
	
	
  }
  \paper {
    linewidth=-1.0;
  }  
  \midi { }
}
