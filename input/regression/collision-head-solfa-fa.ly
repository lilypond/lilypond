
\header {
  texidoc = "
`fa' shape note heads (`fa', `faThin', etc.), which are right
triangles, are merged to avoid creating a rectangular note.

Using property @code{NoteCollision.fa-merge-direction}, the
direction of the merged `fa' can be controlled independently of
the stem direction.  If this property is not set, the `down' glyph
variant is used.
" }

\version "2.23.13"

{
  \clef bass

  \set Staff.shapeNoteStyles = ##(do re mi fa #f la ti)
  <<
    { f2 }
    \\
    { f2 }
  >>
  \override Staff.NoteCollision.fa-merge-direction = #UP
  <<
    { f2 }
    \\
    { f2 }
  >>

  \set Staff.shapeNoteStyles = ##(do re mi faThin #f la ti)
  \override Staff.NoteCollision.fa-merge-direction = #DOWN
  <<
    { f4 }
    \\
    { f4 }
  >>
  \override Staff.NoteCollision.fa-merge-direction = #UP
  <<
    { f4 }
    \\
    { f4 }
  >>
}
