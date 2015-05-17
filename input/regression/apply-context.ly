
\version "2.19.21"


\header {

texidoc = "With @code{\\applyContext}, @code{\\properties} can be modified
procedurally. Applications include: checking bar numbers, smart
octavation.


This example prints a bar-number during processing on stdout.
"

}

\layout { ragged-right= ##t }


\relative {
  c''1 c1

  %% todo: should put something interesting in the .tex output.
  
  \applyContext
  #(lambda (tr)
    (ly:progress "\nWe were called in bar number ~a.\n"
     (ly:context-property tr 'currentBarNumber)))
  c1 c1
}
