\version "2.1.26"
\header {
    
    texidoc = "@cindex Embedded scm

You can embed scheme functions in your scores.  While processing this
file, ``hello world'' is printed to the console.
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


