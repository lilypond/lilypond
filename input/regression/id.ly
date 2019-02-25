\version "2.21.0"

\header {
  texidoc = "Shows the id property of a grob being set.  This should have
no effect.
"
}

{ \override NoteHead.id = "foo" c }
