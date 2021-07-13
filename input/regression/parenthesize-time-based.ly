\version "2.23.4"

\header {
  texidoc = "@code{\\parenthesize} can take the name
of the grob to be parenthesized.  It then acts like
a @code{\\once \\override}."
}

{
  \parenthesize NoteHead
  c'1
  \parenthesize Staff.KeySignature
  \key g \major
  c'1
  \parenthesize Staff.KeyCancellation
  \key c \major
  c'1
}
