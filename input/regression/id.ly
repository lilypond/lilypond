\version "2.19.49"

\header {
  texidoc = "Shows the id property of a grob being set.  This should have
no effect.
"
}

{ \override NoteHead.id = #"foo" c }
