\score { 
  \context Voice \notes\relative c {
    \context Staff \notes\relative c''<
	     \context Voice=one {
		\property Voice.horizontalNoteShift=0
		\property Voice.verticalDirection=1 
		e4 
	     }
	     \context Voice=two {
		\property Voice.verticalDirection=1 
		\property Voice.horizontalNoteShift=1
		cis
	     }
	     \context Voice=three {
		\property Voice.horizontalNoteShift=2
		\property Voice.verticalDirection=1 
		ais
	     }
	     \context Voice=four {
		\property Voice.verticalDirection=-1 
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