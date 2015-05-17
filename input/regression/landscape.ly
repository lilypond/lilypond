\version "2.19.21"
#(set-default-paper-size "a6" 'landscape)
\header { texidoc = " Scores may be printed in landscape mode."}


pattern = \relative { a b c d \break }

\book {    
    \score {
	\new Staff  {
	    \repeat unfold 15 \pattern
	}
    }
}


