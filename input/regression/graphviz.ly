\header {
  texidoc = "The graphviz feature draws dependency graphs for grob properties."

  }

\version "2.11.51"
\include "graphviz-init.ly"

#(whitelist-grob 'NoteHead)
#(whitelist-grob 'Stem)
#(whitelist-grob "NoteHead")
#(whitelist-grob "Stem")

#(map whitelist-symbol '(stencil style duration-log
			 stem-attachment end-position staff-position
			 glyph-name direction))


\book { \score {
  c'4
} }


#(graph-write graph (current-error-port))
#(ly:set-grob-modification-callback #f)
#(ly:set-property-cache-callback #f)

