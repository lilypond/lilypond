
\header {

  texidoc = "Dynamics avoid collision with slur."
}

\version "2.6.0"
\layout {
  indent = 0\mm
  raggedright = ##t
}
\relative
{
  b(^"dyn outside" b f'\p b,)

  g( d'\< d \! g,)
}
