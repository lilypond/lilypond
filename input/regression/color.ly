\version "2.11.51"

\header{
  texidoc = "Each grob can have a color assigned to it.
Use the @code{\override} and @code{\revert} expressions to set the
@code{color} property."
}

\paper { ragged-right = ##t }

\relative {
  \override Accidental #'color = #darkgreen
  \override Beam #'color = #cyan
  \override NoteHead #'color = #darkyellow
  c4
  \override NoteHead #'color = #red
  f
  \override NoteHead #'color = #darkmagenta
  g
  \override NoteHead #'color = #darkblue
  b
  \override NoteHead #'color = #green
  \override Stem #'color = #blue
  e8 es d dis e4 r
}
