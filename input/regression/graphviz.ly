\version "2.16.0"
\include "graphviz-init.ly"

\header {
  texidoc = "The graphviz feature draws dependency graphs for grob properties."
}

#(whitelist-grob 'NoteHead)
#(whitelist-grob 'Stem)
#(whitelist-grob 'Flag)

#(for-each whitelist-symbol
   '(stencil style duration-log stem-attachment end-position staff-position
     glyph-name direction))


\book {
  \score {
    c'4
  }
}

#(ly:progress (call-with-output-string
  (lambda (port) (graph-write graph port))))
#(ly:set-grob-modification-callback #f)
#(ly:set-property-cache-callback #f)
