
\version "2.3.16"

\header {

    texidoc = "In french style beaming, the stems do not go between beams."

}

\score { 
	 \relative c'
	 {
	     \override Stem  #'french-beaming = ##t
	     c16[ c c c]
	     c8[ c16 c16 c8]
	     c8[ c16 e16 g8]
	 }

    \paper { raggedright= ##t }

     }


