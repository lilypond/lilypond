\version "2.1.26"
\header { texidoc = "@cindex Horizontal Shift
You can manually shift notes horizontally. " }

shiftI = \override NoteColumn  #'horizontal-shift = #0
shiftII = \override NoteColumn  #'horizontal-shift = #1
shiftIII = \override NoteColumn  #'horizontal-shift = #2
shiftIV = \override NoteColumn  #'horizontal-shift = #3
shiftV = \override NoteColumn  #'horizontal-shift = #4

\score { 
  \context Voice \notes\relative c {
    
	\context Staff <<
		\new Voice  {\stemUp \shiftI g'' }
		\new Voice  {\stemUp \shiftII e }
		\new Voice  {\stemUp \shiftIII c }
		\new Voice  {\stemUp \shiftIV a }
		\new Voice  {\stemUp \shiftV f }
	>>	
  }
  \paper { raggedright = ##t }  
}

