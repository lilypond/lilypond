\version "2.4.0"

\header {

texidoc =" The A is atop an invisible barline. The barline,
although invisible, is also translated because it is the last one of
the break alignment."
    
}

\layout { raggedright= ##t }

\score{  {
a a a a \break
\mark A
a a a a}}

