\version "1.3.146"

\score { 
  \context Voice \notes\relative c {
    
	\context Staff <
		\context Voice = VA {
			\property Voice.NoteColumn \override #'forced-hshift = #0.1
			\stemUp
	       		\property Voice.NoteColumn \override #'horizontal-shift = #1
			<g' d'>
		}
		\context Voice = VB {
			\stemDown
	       		\property Voice.NoteColumn \override #'horizontal-shift = #1
			\property Voice.NoteColumn \override #'forced-hshift = #-0.1
			<bes f'>
		}
	>
  }
  \paper {
    linewidth=-1.0
  }  
  \midi { }
}
