
\header {

  texidoc = "Dynamics avoid collision with slur."
}

\version "2.11.51"
\layout {
  indent = 0\mm
  ragged-right = ##t
}
\relative
{
  b(^"dyn outside" b f'\p b,)

  g( d'\< d \! g,)
}
