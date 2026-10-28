\version "2.27.5"

\header { texidoc = "Spanners ending on the final musical moment don't
lead to a programming error.  Examples here are a text spanner and a
crescendo spanner." }

#(ly:set-option 'warning-as-error #t)

{
  e'1\startTextSpan
  c'1
  <>\stopTextSpan
}

{
  c'2.\<
  <>\!
}
