
\version "2.3.16"


\header {

texidoc = "With @code{\\applycontext}, @code{\\properties} can be modified
procedurally. Applications include: checking bar numbers, smart
octavation.


This example prints a bar-number during processing on stdout.
"

}

    \paper { raggedright= ##t }


\score {  \relative c'' {
    c1 c1

    %% todo: should put something interesting in the .tex output.
    
    \applycontext
	#(lambda (tr)
	 (format #t "\nWe were called in barnumber ~a.\n"
	  (ly:context-property tr 'currentBarNumber)))
    c1 c1
}}
