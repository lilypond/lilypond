#(ly:set-option 'old-relative)
\version "2.1.7"
\header { texidoc = "@cindex Horizontal Shift
You can manually shift notes horizontally. " }

shiftI = \property Voice.NoteColumn \override #'horizontal-shift = #0
shiftII = \property Voice.NoteColumn \override #'horizontal-shift = #1
shiftIII = \property Voice.NoteColumn \override #'horizontal-shift = #2
shiftIV = \property Voice.NoteColumn \override #'horizontal-shift = #3
shiftV = \property Voice.NoteColumn \override #'horizontal-shift = #4

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

