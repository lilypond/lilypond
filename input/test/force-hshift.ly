\version "1.3.93";
\score { 
  \context Voice \notes\relative c {
    
	\context Staff <
		\context Voice = VA {
			\property Voice.forceHorizontalShift = #0.1
			\stemUp
	       		\property Voice.horizontalNoteShift=1
			<g' d'>
		}	
	
		\context Voice = VB {
			\stemDown
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
