



shiftI = \property Voice.NoteColumn \override #'horizontal-shift = #0
shiftII = \property Voice.NoteColumn \override #'horizontal-shift = #1
shiftIII = \property Voice.NoteColumn \override #'horizontal-shift = #2
shiftIV = \property Voice.NoteColumn \override #'horizontal-shift = #3
shiftV = \property Voice.NoteColumn \override #'horizontal-shift = #4

\score { 
  \context Voice \notes\relative c {
    
	\context Staff <
		\context Voice =VA  {\stemUp \shiftI g'' }
		\context Voice =VB  {\stemUp \shiftII e }
		\context Voice =VC  {\stemUp \shiftIII c }
		\context Voice =VD  {\stemUp \shiftIV a }
		\context Voice =VE  {\stemUp \shiftV f }
	>	
  }
  \paper { }  
  \midi { }
}
