\version "2.12.0"

\header {

  texidoc =" The A is atop an invisible barline. The barline,
although invisible, is also translated because it is the last one of
the break alignment."
  
}

\layout { ragged-right= ##t }

{
  a a a a \break
  \mark A
  a a a a
}

