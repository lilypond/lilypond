\header {

texidoc = "with \\applycontext, \\properties can be modified
procedurally. Applications include: checking bar numbers, smart
octavation. "

}

\score { \notes \relative c'' {
    c1 c1

    %% todo: should put something interesting in the .tex output.
    
    \applycontext #(lambda (tr)
		    (let* ((bn (ly:get-context-property tr 'currentBarNumber)))
		     (if (= bn  3)
		      #t
		      (format #t "We were called in  ~a" bn))
		 ))

    c1 c1
}}
