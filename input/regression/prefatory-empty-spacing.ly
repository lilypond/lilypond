\version "2.7.13"

\header {

  texidoc =" The A is atop an invisible barline. The barline,
although invisible, is also translated because it is the last one of
the break alignment."
  
}

\layout { raggedright= ##t }

{
  a a a a \break
  \mark A
  a a a a
}

