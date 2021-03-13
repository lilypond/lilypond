\version "2.16.0"
\include "graphviz-init.ly"

\header {
  texidoc = "The graphviz feature draws dependency graphs for grob properties."
}

whitelists = #'((grob . (NoteHead Stem Flag))
                (symbol . (stencil style duration-log stem-attachment
                           end-position staff-position glyph-name direction)))

blacklists = #'()

\graphvizSetupCallbacks #'(mod cache) #'() #whitelists #blacklists

\book {
  \score {
    c'4
  }
}

\graphvizWriteGraph #(current-error-port)
% The graph is written to stderr and therefore becomes part
% of the normal program output. This is done here to get coverage
% in the regression tests. To split normal progress messages and
% graphviz output, set the output port to stdout instead:
% \graphvizWriteGraph #(current-output-port)
\graphvizReleaseCallbacks
