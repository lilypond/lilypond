\score { 
  \context Voice \notes\relative c {
    
	\context Staff <
		\context Voice = VA {
			\property Voice.forceHorizontalShift = #0.1
			\stemup
	       		\property Voice.horizontalNoteShift=1
			<g' d'>
		}	
	
		\context Voice = VB {
			\stemdown
	       		\property Voice.horizontalNoteShift=1
			\property Voice.forceHorizontalShift = #-0.1
			<bes f'>
		}
	>
	
  }
  \paper {
    linewidth=-1.0;
  }  
  \midi { }
}