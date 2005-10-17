
\header {

  texidoc = "Dynamics avoid collision with slur."
}

\version "2.7.13"
\layout {
  indent = 0\mm
  raggedright = ##t
}
\relative
{
  b(^"dyn outside" b f'\p b,)

  g( d'\< d \! g,)
}
