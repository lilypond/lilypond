
\version "2.1.26"

\header{ texidoc = "@cindex Stem Length
You can alter the length of stems. "
}

\score { 
  \context Voice \notes\relative c {
	g''4 \override Stem  #'length = #14  g4
	\override Stem  #'length = #3 g4  g,,4  
  }
  \paper { raggedright = ##t }  
}

