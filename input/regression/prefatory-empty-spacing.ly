\version "2.1.36"

\header {

texidoc =" The A is atop an invisible barline. The barline,
although invisible, is also translated because it is the last one of
the break alignment."
    
}

\paper { raggedright= ##t }

\score{ \notes {
a a a a \break
\mark A
a a a a}}

