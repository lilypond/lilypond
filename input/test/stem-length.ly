
\version "2.3.8"

\header{ texidoc = "@cindex Stem Length
The length of stems can be altered. "
}

\score { 
  \context Voice \relative c {
	g''4 \override Stem  #'length = #14  g4
	\override Stem  #'length = #3 g4  g,,4  
  }
  \paper { raggedright = ##t }  
}

