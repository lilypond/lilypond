\version "2.15.27"

\header {
  texidoc = "Shows the id property of a grob being set.  This should have
no effect in the PS backend.
"
}

{ \override NoteHead #'id = #"foo" c }
