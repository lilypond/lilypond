\version "2.19.12"

\header {
  texidoc = "The @code{Script} grobs should follow the descending melody line,
even though the @code{NoteHead} stencils are point stencils. The
@code{Stem_engraver} is removed so that the only
@code{side-support-element} is the @code{NoteHead}.
"
}

\layout {
  \context {
    \Voice
    \remove "Stem_engraver"
  }
}

{
  \override Script.direction = #DOWN
  \override NoteHead.stencil = #point-stencil
  c'2.-> b8-- a-- g1->
}
