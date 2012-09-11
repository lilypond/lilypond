\header
{

  texidoc = "left attach dir for text crescendi starting on an
  absolute dynamic is changed, so cresc. and the absolute dynamic
  don't overstrike."
  
}

\version "2.16.0"
\paper { ragged-right = ##t }

\relative c' {
  \crescTextCresc
  c4\p\< c c c\!
}
