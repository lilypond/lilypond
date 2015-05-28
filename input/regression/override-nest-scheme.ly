\version "2.19.21"

\header {
  texidoc = "A sublist of grob property lists may be overridden within a callback.
  This test uses a custom stencil callback which changes the Y coordinate
  of the right bound of the glissando spanner."
}

\relative {
  \override Glissando.after-line-breaking =
    #(lambda (grob)
       (ly:grob-set-nested-property! grob '(bound-details right Y) 3))
  c'1 \glissando
  d1
}

