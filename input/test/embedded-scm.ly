\version "2.2.0"
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
	\notes\relative c' { c }
	\paper {raggedright = ##t}
}


