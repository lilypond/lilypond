\version "2.3.4"
\header { texidoc = "@cindex Horizontal Shift
Notes may be manually horizontally shifted. " }

shiftI = \override NoteColumn  #'horizontal-shift = #0
shiftII = \override NoteColumn  #'horizontal-shift = #1
shiftIII = \override NoteColumn  #'horizontal-shift = #2
shiftIV = \override NoteColumn  #'horizontal-shift = #3
shiftV = \override NoteColumn  #'horizontal-shift = #4

\score { 
  \context Voice \relative c {
    
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

