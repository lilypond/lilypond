\version "2.7.39"
\header {
    
    texidoc = "@cindex Embedded scm

You can embed scheme functions in your scores.  While generating the
output, ``hello world'' is printed to the console.
"
     % see also: --safe-mode
}

#(begin
  (newline)
  (display "hello world")
  (newline))

\score {
	\relative c' { c }
	\layout {ragged-right = ##t}
}


