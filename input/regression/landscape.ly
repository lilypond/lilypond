
#(set-default-paper-size "a6" 'landscape)

\version "2.4.6"
pattern = \relative { a b c d \break }

\book {    
    \score {
	\new Staff  {
	    \repeat unfold 15 \pattern
	}
    }
}


