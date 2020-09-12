\version "2.21.6"

\header {
  texidoc = "Score level Vertical_align_engraver ignore axis
  groups that are not spanners.  In this case, the @code{Devnull}
  context has no Axis_group_engraver, so the NoteColumn appears like a
  parent-less axis group; even so, the Score level alignment ignores
  it."
}

<<
  \new Devnull = "a" { s }
  \new Staff { c' \change Staff = "a" c' }
>>
