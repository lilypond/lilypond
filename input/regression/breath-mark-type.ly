\version "2.23.9"

\header {
  texidoc="The @code{breathMarkType} context property controls the
sign that @code{\\breathe} produces.  The output should show two
default breathing signs then two tick marks (check marks)."
}

\fixed c' {
  b4 b
  \breathe
  b4 b
  \breathe

  b4 b
  \set breathMarkType = #'tickmark
  \breathe
  b4 b
  \breathe
}
