\version "2.3.16"
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
	\paper {raggedright = ##t}
}


