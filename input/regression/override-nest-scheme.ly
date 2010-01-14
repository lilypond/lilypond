\version "2.13.11"

\header {
  texidoc = "A sublist of grob property lists may be overridden within a callback.
  This test uses a custom stencil callback which changes the Y coordinate
  of the right bound of the glissando spanner."
}

\relative c' {
  \override Glissando #'after-line-breaking =
    #(lambda (grob)
       (ly:grob-set-nested-property! grob '(bound-details right Y) 3))
  c1 \glissando
  d1
}

