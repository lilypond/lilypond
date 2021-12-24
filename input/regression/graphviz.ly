\version "2.23.6"
\include "graphviz-init.ly"

\header {
  texidoc = "The graphviz feature draws dependency graphs for grob properties."
}

whitelists = #'((grob . (NoteHead Stem Flag))
                (symbol . (stencil style duration-log stem-attachment
                           end-position staff-position glyph-name direction)))

blacklists = #'()

label-formatting = #default-label-formatting
label-formatting.mod =
  #`("~a\\n~a\\n~a <- ~a"
     (,grob::name ,identity ,discard ,discard ,identity ,truncate-value)
     ,escape-label)

\graphvizSetupCallbacks #'(mod cache) #label-formatting #whitelists #blacklists

\book {
  \score {
    c'1
  }
}

\graphvizWriteGraph #(current-error-port)
% The graph is written to stderr and therefore becomes part
% of the normal program output. This is done here to get coverage
% in the regression tests. To split normal progress messages and
% graphviz output, set the output port to stdout instead:
% \graphvizWriteGraph #(current-output-port)
\graphvizReleaseCallbacks
