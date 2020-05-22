\version "2.21.2"

\header{
  texidoc = "Each grob can have a color assigned to it.
Use the @code{\\override} and @code{\\revert} expressions to set the
@code{color} property.

Colors may include an alpha channel, but that is only apparent in
SVG output.
"
}

\paper { ragged-right = ##t }

\relative {
  \override Accidental.color = #darkgreen
  \override Beam.color = #cyan
  \override NoteHead.color = #darkyellow
  c'4
  \override NoteHead.color = #red
  f
  \override NoteHead.color = #darkmagenta
  g
  \override NoteHead.color = #darkblue
  b
  \override NoteHead.color = #green
  \override Stem.color = #blue
  \override Flag.color = #magenta
  e8 es d dis e4 r
}

\layout {
  \context {
    \Staff
    %% Alpha transparency is enabled in SVG output.
    \override Clef.color = #(rgb-color 0 0 0 0.5)
  }
}
