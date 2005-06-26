
\version "2.6.0"


\header {
    texidoc = 
    "Beaming can be overidden for individual stems."
}
\score {
     \relative c'' {
	c32[ c

	     %% WARNING: #'beaming is written, so this
	     %% property can not be shared between objects. Always use
	     %%  \once.
	     %%

	     \once \override Stem 
	     #'beaming = #(cons (list   1 2) (list 0 2 4))
	     c
	     \once \override Stem 
	     #'beaming = #(cons (list 0 2 4) (list 0 1 4))
	     c c c]
    }
    \layout{ 
	raggedright = ##t 
    }
}
